package erlang

import (
	"log"
	"net/url"
	"path"
	"path/filepath"
	"sort"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

var importFuncs = map[string]func(args language.ImportReposArgs) language.ImportReposResult{
	"metadata.config": importReposFromHexMetadata,
	"rebar.config":    importReposFromRebarConfig,
}

func (*erlangLang) UpdateRepos(args language.UpdateReposArgs) language.UpdateReposResult {
	Log(args.Config, "UpdateRepos:", args)
	return language.UpdateReposResult{}
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

func importReposFromRebarConfig(args language.ImportReposArgs) language.ImportReposResult {
	dir, f := filepath.Split(args.Path)
	parser := newRebarConfigParser(dir, "")
	rebarConfig, err := parser.parseRebarConfig(f)
	if err != nil {
		log.Fatalf("ERROR: %v\n", err)
	}

	gen := []*rule.Rule{}

	for _, dep := range rebarConfig.Deps {
		u, err := url.Parse(dep["remote"])
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}
		if dep["kind"] == "git" && u.Host == "github.com" {
			org := path.Base(path.Dir(u.Path))
			r := rule.NewRule(githubErlangAppKind, dep["name"])
			r.SetAttr("org", org)
			r.SetAttr("ref", dep["ref"])
			r.SetAttr("version", dep["ref"])
			r.SetAttr("erlc_opts", []string{
				"+deterministic",
				"+debug_info",
			})
			gen = append(gen, r)
		} else {
			Log(args.Config, "    Skipping", dep)
		}
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
