package erlang

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path"
	"path/filepath"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
	"github.com/emirpasic/gods/sets/treeset"
	godsutils "github.com/emirpasic/gods/utils"
)

const (
	hexContentsArchiveFilename = "contents.tar.gz"
	hexMetadataFilename        = "metadata.config"
)

var (
	hexPmFiles = []string{
		"VERSION",
		"CHECKSUM",
		hexMetadataFilename,
		hexContentsArchiveFilename,
	}
)

const (
	rebarConfigFilename = "rebar.config"
)

var (
	ignoredDirs = []string{
		".git",
		".github",
	}
)

func erlcOptsWithSelect(debugOpts []string) rule.SelectStringListValue {
	var defaultOpts []string
	if Contains(debugOpts, "+deterministic") {
		defaultOpts = debugOpts
	} else {
		defaultOpts = append(debugOpts, "+deterministic")
	}
	return rule.SelectStringListValue{
		"@rules_erlang//:debug_build": debugOpts,
		"//conditions:default":        defaultOpts,
	}
}

func filterOutDirectories(files []string) []string {
	dirs := make([]string, len(files))
	for i, f := range files {
		dirs[i] = filepath.Dir(f)
	}
	filtered := []string{}
	for _, f := range files {
		if !Contains(dirs, f) {
			filtered = append(filtered, f)
		}
	}
	return filtered
}

type erlangApp struct {
	Name         string
	Description  string
	Version      string
	Srcs         *treeset.Set
	PrivateHdrs  *treeset.Set
	PublicHdrs   *treeset.Set
	AppSrc       *treeset.Set
	LicenseFiles *treeset.Set
	ErlcOpts     []string
	Deps         *treeset.Set
	ExtraApps    *treeset.Set
}

func newErlangApp() *erlangApp {
	return &erlangApp{
		ErlcOpts:  []string{"+debug_info"},
		Deps:      treeset.NewWith(godsutils.StringComparator),
		ExtraApps: treeset.NewWith(godsutils.StringComparator),
	}
}

func isHexPmTar(regularFiles []string) bool {
	return ContainsAll(regularFiles, hexPmFiles)
}

func isRebar(regularFiles []string) bool {
	return Contains(regularFiles, rebarConfigFilename)
}

func guessKind(f string, erlangApp *erlangApp) {
	if strings.HasPrefix(f, "src/") {
		if strings.HasSuffix(f, ".erl") {
			erlangApp.Srcs.Add(f)
		} else if strings.HasSuffix(f, ".hrl") {
			erlangApp.PrivateHdrs.Add(f)
		} else if strings.HasSuffix(f, ".app.src") {
			erlangApp.AppSrc.Add(f)
		}
	} else if strings.HasPrefix(f, "include/") {
		if strings.HasSuffix(f, ".hrl") {
			erlangApp.PublicHdrs.Add(f)
		}
	} else if strings.HasPrefix(f, "LICENSE") {
		erlangApp.LicenseFiles.Add(f)
	}
}

func moduleName(src string) string {
	return strings.TrimSuffix(filepath.Base(src), ".erl")
}

func beamFile(src string) string {
	r := "ebin/" + strings.TrimPrefix(src, "src/")
	return strings.TrimSuffix(r, ".erl") + ".beam"
}

func importHexPmTar(args language.GenerateArgs, result *language.GenerateResult, erlangApp *erlangApp) (string, error) {
	Log(args.Config, "    Hex.pm archive detected")

	hexMetadataParser := newHexMetadataParser(args.Config.RepoRoot, args.Rel)

	hexMetadata, err := hexMetadataParser.parseHexMetadata(hexMetadataFilename)
	if err != nil {
		return "", err
	}

	Log(args.Config, "    hexMetadata:", hexMetadata)

	erlangApp.Name = hexMetadata.Name
	erlangApp.Description = hexMetadata.Description
	erlangApp.Version = hexMetadata.Version

	untar := rule.NewRule("untar", "contents")
	untar.SetAttr("archive", hexContentsArchiveFilename)
	untar.SetAttr("outs", filterOutDirectories(hexMetadata.Files))

	result.Gen = append(result.Gen, untar)
	result.Imports = append(result.Imports, untar.PrivateAttr(config.GazelleImportsKey))

	erlangApp.Srcs = treeset.NewWith(godsutils.StringComparator)
	erlangApp.PrivateHdrs = treeset.NewWith(godsutils.StringComparator)
	erlangApp.PublicHdrs = treeset.NewWith(godsutils.StringComparator)
	erlangApp.AppSrc = treeset.NewWith(godsutils.StringComparator)
	erlangApp.LicenseFiles = treeset.NewWith(godsutils.StringComparator)

	for _, f := range hexMetadata.Files {
		guessKind(f, erlangApp)
	}

	for _, req := range hexMetadata.Requirements {
		if !req.Optional {
			erlangApp.Deps.Add(req.App)
		}
	}

	// extract to a temporary directory
	extractedContentsDir, err := ioutil.TempDir("", hexMetadata.Name)
	if err != nil {
		return "", err
	}
	Log(args.Config, "    tempDir:", extractedContentsDir)

	hexContentsArchivePath := filepath.Join(args.Config.RepoRoot, args.Rel, hexContentsArchiveFilename)
	Log(args.Config, "    hexContentsArchivePath:", hexContentsArchivePath)
	err = ExtractTarGz(hexContentsArchivePath, extractedContentsDir)
	if err != nil {
		return "", err
	}

	if Contains(hexMetadata.BuildTools, "rebar3") {
		err = importRebar(args, extractedContentsDir, erlangApp)
		if err != nil {
			return "", err
		}
	}

	return extractedContentsDir, nil
}

func importRebar(args language.GenerateArgs, rebarAppPath string, erlangApp *erlangApp) error {
	Log(args.Config, "    Importing Rebar from", filepath.Join(rebarAppPath, rebarConfigFilename))

	rebarConfigParser := newRebarConfigParser(rebarAppPath, "")
	rebarConfig, err := rebarConfigParser.parseRebarConfig(rebarConfigFilename)
	if err != nil {
		return err
	}

	erlangApp.ErlcOpts = make([]string, len(rebarConfig.ErlOpts))
	for i, o := range rebarConfig.ErlOpts {
		erlangApp.ErlcOpts[i] = "+" + o
	}

	if erlangApp.Srcs == nil {
		erlangApp.Srcs = treeset.NewWith(godsutils.StringComparator)
		erlangApp.PrivateHdrs = treeset.NewWith(godsutils.StringComparator)
		erlangApp.PublicHdrs = treeset.NewWith(godsutils.StringComparator)
		erlangApp.AppSrc = treeset.NewWith(godsutils.StringComparator)
		erlangApp.LicenseFiles = treeset.NewWith(godsutils.StringComparator)

		err := filepath.Walk(rebarAppPath,
			func(path string, info os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if info.IsDir() && Contains(ignoredDirs, filepath.Base(path)) {
					return filepath.SkipDir
				}
				rel, err := filepath.Rel(rebarAppPath, path)
				if err != nil {
					return err
				}
				guessKind(rel, erlangApp)
				return nil
			})
		if err != nil {
			log.Println(err)
		}

		for _, dep := range rebarConfig.Deps {
			erlangApp.Deps.Add(dep["name"])
		}
	}
	return nil
}

func (erlang *erlangLang) GenerateRules(args language.GenerateArgs) language.GenerateResult {
	if args.File != nil {
		Log(args.Config, "GenerateRules:", args.Rel, args.File.Path)
	} else {
		Log(args.Config, "GenerateRules:", args.Rel, args.File)
	}

	var result language.GenerateResult
	result.Gen = make([]*rule.Rule, 0)

	erlangApp := newErlangApp()

	sourcePrefix := filepath.Join(args.Config.RepoRoot, args.Rel)

	if isHexPmTar(args.RegularFiles) {
		prefix, err := importHexPmTar(args, &result, erlangApp)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}
		defer os.RemoveAll(prefix)
		sourcePrefix = prefix
	} else if isRebar(args.RegularFiles) {
		err := importRebar(args, sourcePrefix, erlangApp)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}
	}

	if erlangApp.Srcs == nil {
		return language.GenerateResult{}
	}

	if erlangApp.Name == "" {
		if !erlangApp.AppSrc.Empty() {
			dotAppParser := newDotAppParser(args.Config.RepoRoot, args.Rel)
			dotApp, err := dotAppParser.parseAppSrc(erlangApp.AppSrc.Values()[0].(string))
			if err != nil {
				log.Fatalf("ERROR: %v\n", err)
			}

			erlangApp.Name = strings.TrimSuffix(filepath.Base(erlangApp.AppSrc.Values()[0].(string)), ".app.src")
			props := (*dotApp)[erlangApp.Name]
			erlangApp.ExtraApps = treeset.NewWith(godsutils.StringComparator)
			for _, app := range props.Applications {
				if !Contains([]string{"kernel", "stdlib"}, app) {
					erlangApp.ExtraApps.Add(app)
				}
			}
			erlangApp.ExtraApps = erlangApp.ExtraApps.Difference(erlangApp.Deps)
		} else {
			erlangApp.Name = filepath.Base(sourcePrefix)
		}
	}

	// TODO: detect "extra_apps" from the .app.src if necessary
	//       any 'applications' not in the deps are assumed to be
	//       local/builtin

	Log(args.Config, "    Analyzing sources...")

	erlParser := newErlParser(args.Config.RepoRoot, args.Rel)

	outs := treeset.NewWith(godsutils.StringComparator)
	for i, src := range erlangApp.Srcs.Values() {
		Log(args.Config, "        Parsing", src, "->", filepath.Join(sourcePrefix, src.(string)))
		erlAttrs, err := erlParser.parseErl(filepath.Join(sourcePrefix, src.(string)))
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}

		theseHdrs := []string{}
		for _, include := range erlAttrs.Include {
			for _, h := range erlangApp.PrivateHdrs.Union(erlangApp.PublicHdrs).Values() {
				hdr := h.(string)
				if path.Base(hdr) == include {
					theseHdrs = append(theseHdrs, hdr)
				}
			}
		}

		theseDeps := []string{}
		for _, include := range erlAttrs.IncludeLib {
			for _, d := range erlangApp.Deps.Values() {
				dep := d.(string)
				if strings.HasPrefix(include, dep+"/") {
					theseDeps = append(theseDeps, dep)
				}
			}
		}

		theseBeam := []string{}
		for _, behaviour := range erlAttrs.Behaviour {
			found := false
			for _, s := range erlangApp.Srcs.Values() {
				src := s.(string)
				if moduleName(src) == behaviour {
					theseBeam = append(theseBeam, beamFile(src))
					found = true
					break
				}
			}
			if !found {
				erlangConfig := args.Config.Exts[languageName].(ErlangConfig)
				if dep, found := erlangConfig.BehaviourMappings[behaviour]; found {
					theseDeps = append(theseDeps, dep)
				}
			}
			if !found {
				Log(args.Config, "    ", behaviour, "source not found, skipping.")
			}
		}

		Log(args.Config, src, erlAttrs)

		out := "ebin/" + strings.TrimPrefix(src.(string), "src/")
		out = strings.TrimSuffix(out, ".erl") + ".beam"
		outs.Add(out)

		erlang_bytecode := rule.NewRule("erlang_bytecode", fmt.Sprintf("beam_files_%d", i))
		erlang_bytecode.SetAttr("srcs", []interface{}{src})
		if len(theseHdrs) > 0 {
			erlang_bytecode.SetAttr("hdrs", theseHdrs)
		}
		erlang_bytecode.SetAttr("erlc_opts", erlcOptsWithSelect(erlangApp.ErlcOpts))
		erlang_bytecode.SetAttr("outs", []string{out})
		if len(theseBeam) > 0 {
			erlang_bytecode.SetAttr("beam", theseBeam)
		}
		if len(theseDeps) > 0 {
			erlang_bytecode.SetAttr("deps", theseDeps)
		}

		result.Gen = append(result.Gen, erlang_bytecode)
		result.Imports = append(result.Imports, erlang_bytecode.PrivateAttr(config.GazelleImportsKey))
	}

	app_file := rule.NewRule("app_file", "app_file")
	if erlangApp.Description != "" {
		app_file.SetAttr("app_description", erlangApp.Description)
	}
	app_file.SetAttr("app_name", erlangApp.Name)
	app_file.SetAttr("app_src", erlangApp.AppSrc.Values())
	if erlangApp.Version != "" {
		app_file.SetAttr("app_version", erlangApp.Version)
	}
	app_file.SetAttr("out", filepath.Join("ebin", erlangApp.Name+".app"))
	app_file.SetAttr("modules", outs.Values())
	if !erlangApp.ExtraApps.Empty() {
		app_file.SetAttr("extra_apps", erlangApp.ExtraApps.Values())
	}
	if !erlangApp.Deps.Empty() {
		app_file.SetAttr("deps", erlangApp.Deps.Values())
	}

	result.Gen = append(result.Gen, app_file)
	result.Imports = append(result.Imports, app_file.PrivateAttr(config.GazelleImportsKey))

	erlang_app_info := rule.NewRule("erlang_app_info", erlangApp.Name)
	erlang_app_info.SetAttr("srcs", erlangApp.Srcs.Union(erlangApp.PrivateHdrs).Union(erlangApp.PublicHdrs).Union(erlangApp.AppSrc).Values())
	erlang_app_info.SetAttr("hdrs", erlangApp.PublicHdrs.Values())
	erlang_app_info.SetAttr("app", ":"+app_file.Name())
	erlang_app_info.SetAttr("app_name", erlangApp.Name)
	erlang_app_info.SetAttr("beam", outs.Values())
	erlang_app_info.SetAttr("license_files", erlangApp.LicenseFiles.Values())
	erlang_app_info.SetAttr("visibility", []string{"//visibility:public"})
	if !erlangApp.Deps.Empty() {
		erlang_app_info.SetAttr("deps", erlangApp.Deps.Values())
	}

	result.Gen = append(result.Gen, erlang_app_info)
	result.Imports = append(result.Imports, erlang_app_info.PrivateAttr(config.GazelleImportsKey))

	alias := rule.NewRule("alias", "erlang_app")
	alias.SetAttr("actual", ":"+erlang_app_info.Name())
	alias.SetAttr("visibility", []string{"//visibility:public"})

	result.Gen = append(result.Gen, alias)
	result.Imports = append(result.Imports, alias.PrivateAttr(config.GazelleImportsKey))

	return result
}
