package erlang

import (
	"bytes"
	"context"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
	"github.com/bazelbuild/rules_go/go/tools/bazel"
)

const (
	colorReset = "\033[0m"
	colorRed   = "\033[31m"
)

var importFuncs = map[string]func(args language.ImportReposArgs) language.ImportReposResult{
	"metadata.config": importReposFromHexMetadata,
	"rebar.lock":      importReposFromRebarLock,
}

func ruleForHexPackage(config *config.Config, pkg, version string) (*rule.Rule, error) {
	nameDashVersion := pkg + "-" + version
	downloadDir, err := ioutil.TempDir("", nameDashVersion)
	if err != nil {
		return nil, err
	}
	// defer os.RemoveAll(downloadDir)

	archivePath := filepath.Join(downloadDir, nameDashVersion+".tar")
	err = DownloadRelease(pkg, version, archivePath)
	if err != nil {
		return nil, err
	}

	extractedPackageDir := filepath.Join(downloadDir, nameDashVersion)
	if err := os.MkdirAll(extractedPackageDir, 0755); err != nil {
		return nil, err
	}
	Log(config, "    extracting to", extractedPackageDir)
	err = ExtractTar(archivePath, extractedPackageDir)
	if err != nil {
		return nil, err
	}

	// TODO: add an optional flag to tell update-repos what to recurse with
	gazelleRunfile, err := bazel.Runfile("gazelle")
	if err != nil {
		return nil, err
	}

	ctx := context.Background()
	cmd := exec.CommandContext(ctx, gazelleRunfile)

	cmd.Args = append(cmd.Args,
		"--no_tests",
		"-repo_root", extractedPackageDir,
		extractedPackageDir)
	output, err := cmd.CombinedOutput()
	if err != nil {
		fmt.Println("gazelle failed for", pkg, "in", extractedPackageDir)
		if err != nil {
			fmt.Println(string(colorRed))
			fmt.Println(bytes.NewBuffer(output).String())
			fmt.Println(string(colorReset))
		}
		return nil, err
	}

	// Copy the BUILD.bazel from the project to this repo and reference
	// it in the rule

	r := rule.NewRule(hexPmErlangAppKind, pkg)
	// if pkg.Name != pkg.Pkg {
	// 	r.SetAttr("pkg", pkg.Pkg)
	// }
	r.SetAttr("version", version)

	return r, nil
}

func (*erlangLang) UpdateRepos(args language.UpdateReposArgs) language.UpdateReposResult {
	Log(args.Config, "UpdateRepos:", args.Imports)

	result := language.UpdateReposResult{}

	for _, imp := range args.Imports {
		// TODO: allow name=hex.pm/pkg@version as well
		if strings.HasPrefix(imp, "hex.pm/") {
			pkg := strings.TrimPrefix(imp, "hex.pm/")
			var version string
			if strings.Contains(pkg, "@") {
				parts := strings.SplitN(pkg, "@", 2)
				pkg = parts[0]
				version = parts[1]
			} else {
				Log(args.Config, "    will fetch latest", pkg, "from hex.pm")
				var err error
				version, err = LatestRelease(pkg)
				if err != nil {
					log.Fatalf("ERROR: %v\n", err)
					continue
				}
			}
			Log(args.Config, "    will fetch", pkg, version, "from hex.pm")
			release, err := GetRelease(pkg, version)
			if err != nil {
				log.Fatalf("ERROR: %v\n", err)
				continue
			}

			r, err := ruleForHexPackage(args.Config, pkg, version)
			if err != nil {
				log.Fatalf("ERROR: %v\n", err)
				continue
			}
			result.Gen = append(result.Gen, r)

			if len(release.Requirements) > 0 {
				fmt.Printf("%s@%s requirements:\n", pkg, version)
				for _, req := range release.Requirements {
					var optPart string
					if req.Optional {
						optPart = " (Optional)"
					}
					fmt.Printf("    %s %s%s\n", req.App, req.Requirement, optPart)
					fmt.Println()
				}
				fmt.Println("If these requirements are not in the workspace, re-run 'gazelle update-repos hex.pm/[dep]@[version]' to add them.")	
			}
		} else {
			Log(args.Config, "    ignoring", imp, "as its prefix is not recognized.")
			Log(args.Config, "    only hex.pm/ is currently supported.")
		}
	}

	return result
}

func (*erlangLang) CanImport(path string) bool {
	return importFuncs[filepath.Base(path)] != nil
}

func importReposFromHexMetadata(args language.ImportReposArgs) language.ImportReposResult {
	dir, f := filepath.Split(args.Path)
	parser := newHexMetadataParser(dir, "")
	hexMetadata, err := parser.parseHexMetadata(f)
	if err != nil {
		log.Fatalf("ERROR: %v\n", err)
	}

	gen := []*rule.Rule{}

	for _, req := range hexMetadata.Requirements {
		if req.Repository == "hexpm" {
			r := rule.NewRule(hexPmErlangAppKind, req.Name)
			r.SetAttr("version", minVersion(req.Requirement))
			r.SetAttr("erlc_opts", []string{
				"+deterministic",
				"+debug_info",
			})
			gen = append(gen, r)
		}
	}

	return language.ImportReposResult{Gen: gen}
}

func importReposFromRebarLock(args language.ImportReposArgs) language.ImportReposResult {
	dir, f := filepath.Split(args.Path)
	parser := newRebarConfigParser(dir, "")
	rebarLock, err := parser.parseRebarLock(f)
	if err != nil {
		log.Fatalf("ERROR: %v\n", err)
	}

	gen := []*rule.Rule{}

	for _, pkg := range rebarLock.Pkgs {
		// Should we pull the package somewhere, and run
		// gazelle on the temp dir?

		r := rule.NewRule(hexPmErlangAppKind, pkg.Name)
		if pkg.Name != pkg.Pkg {
			r.SetAttr("pkg", pkg.Pkg)
		}
		r.SetAttr("version", pkg.Version)
		gen = append(gen, r)
	}

	return language.ImportReposResult{Gen: gen}
}

func (*erlangLang) ImportRepos(args language.ImportReposArgs) language.ImportReposResult {
	Log(args.Config, "ImportRepos:", args.Path)
	res := importFuncs[filepath.Base(args.Path)](args)

	sort.SliceStable(res.Gen, func(i, j int) bool {
		return res.Gen[i].Name() < res.Gen[j].Name()
	})

	return res
}

func minVersion(requirement string) string {
	return strings.TrimPrefix(requirement, "~> ")
}
