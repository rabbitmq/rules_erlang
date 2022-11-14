package erlang

import (
	"io/ioutil"
	"log"
	"os"
	"path"
	"path/filepath"
	"sort"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
	"github.com/emirpasic/gods/sets/treeset"
	godsutils "github.com/emirpasic/gods/utils"
	"github.com/google/go-cmp/cmp"
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

const (
	erlcOptsRuleName     = "erlc_opts"
	testErlcOptsRuleName = "test_erlc_opts"
)

const (
	macroFileName = "app.bzl"
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
	TestSrcs     *treeset.Set
	TestHdrs     *treeset.Set
	LicenseFiles *treeset.Set
	ErlcOpts     []string
	TestErlcOpts []string
	Deps         *treeset.Set
	ExtraApps    *treeset.Set
}

func newErlangApp() *erlangApp {
	return &erlangApp{
		Srcs:         treeset.NewWith(godsutils.StringComparator),
		PrivateHdrs:  treeset.NewWith(godsutils.StringComparator),
		PublicHdrs:   treeset.NewWith(godsutils.StringComparator),
		AppSrc:       treeset.NewWith(godsutils.StringComparator),
		TestSrcs:     treeset.NewWith(godsutils.StringComparator),
		TestHdrs:     treeset.NewWith(godsutils.StringComparator),
		LicenseFiles: treeset.NewWith(godsutils.StringComparator),
		ErlcOpts:     []string{"+debug_info"},
		TestErlcOpts: []string{"+debug_info", "-DTEST=1"},
		Deps:         treeset.NewWith(godsutils.StringComparator),
		ExtraApps:    treeset.NewWith(godsutils.StringComparator),
	}
}

func isHexPmTar(regularFiles []string) bool {
	return ContainsAll(regularFiles, hexPmFiles)
}

func isRebar(regularFiles []string) bool {
	return Contains(regularFiles, rebarConfigFilename)
}

func isProbablyBareErlang(args language.GenerateArgs) bool {
	// if there is a src dir with .erl files in it
	if Contains(args.Subdirs, "src") {
		hasErlFiles := false
		err := filepath.Walk(filepath.Join(args.Config.RepoRoot, args.Rel, "src"),
			func(path string, info os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if !info.IsDir() && strings.HasSuffix(filepath.Base(path), ".erl") {
					hasErlFiles = true
					return filepath.SkipDir
				}
				return nil
			})
		if err != nil {
			log.Println(err)
		}
		return hasErlFiles
	}
	return false
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
	} else if strings.HasPrefix(f, "test/") {
		if strings.HasSuffix(f, ".erl") {
			erlangApp.TestSrcs.Add(f)
		} else if strings.HasSuffix(f, ".hrl") {
			erlangApp.TestHdrs.Add(f)
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

func testBeamFile(src string) string {
	r := "test/" + strings.TrimPrefix(src, "src/")
	return strings.TrimSuffix(r, ".erl") + ".beam"
}

func ruleName(f string) string {
	r := strings.ReplaceAll(f, string(filepath.Separator), "_")
	return strings.ReplaceAll(r, ".", "_")
}

func importHexPmTar(args language.GenerateArgs, result *language.GenerateResult, erlangApp *erlangApp) (string, error) {
	Log(args.Config, "    Importing Hex.pm archive from", filepath.Join(args.Config.RepoRoot, args.Rel))

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

	if rebarConfig.ErlOpts != nil {
		erlangApp.ErlcOpts = make([]string, len(*rebarConfig.ErlOpts))
		for i, o := range *rebarConfig.ErlOpts {
			erlangApp.ErlcOpts[i] = "+" + o
		}
	}

	if erlangApp.Srcs.Empty() {
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

func importBareErlang(args language.GenerateArgs, erlangApp *erlangApp) error {
	appPath := filepath.Join(args.Config.RepoRoot, args.Rel)
	Log(args.Config, "    Importing bare erlang from", appPath)

	err := filepath.Walk(appPath,
		func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			if info.IsDir() {
				if Contains(ignoredDirs, filepath.Base(path)) {
					return filepath.SkipDir
				} else {
					return nil
				}
			}
			rel, err := filepath.Rel(appPath, path)
			if err != nil {
				return err
			}
			guessKind(rel, erlangApp)
			return nil
		})
	if err != nil {
		log.Println(err)
	}

	return nil
}

func updateRules(f *rule.File, rules []*rule.Rule, filename string) {
	oldToNew := make(map[*rule.Rule]*rule.Rule)
	var new []*rule.Rule

	for _, newRule := range rules {
		matched := false
		newSrcs := newRule.AttrStrings("srcs")
		for _, oldRule := range f.Rules {
			oldSrcs := oldRule.AttrStrings("srcs")
			if cmp.Equal(oldSrcs, newSrcs) {
				oldToNew[oldRule] = newRule
				matched = true
				break
			}
		}
		if !matched {
			new = append(new, newRule)
		}
	}
	for _, oldRule := range f.Rules {
		if newRule, ok := oldToNew[oldRule]; ok {
			rule.MergeRules(newRule, oldRule, erlangKinds[erlangBytecodeKind].MergeableAttrs, filename)
		} else {
			oldRule.Delete()
		}
	}
	for _, newRule := range new {
		newRule.Insert(f)
	}
	sort.SliceStable(f.Rules, func(i, j int) bool {
		return f.Rules[i].Name() < f.Rules[j].Name()
	})
}

func ensureLoad(name, symbol string, index int, f *rule.File) {
	needsLoad := true
	for _, load := range f.Loads {
		if load.Name() == name {
			needsLoad = false
			if !Contains(load.Symbols(), symbol) {
				load.Add(symbol)
			}
		}
	}
	if needsLoad {
		l := rule.NewLoad(name)
		l.Add(symbol)
		l.Insert(f, index)
	}
}

func (erlang *erlangLang) GenerateRules(args language.GenerateArgs) language.GenerateResult {
	if args.File != nil {
		Log(args.Config, "GenerateRules:", args.Rel, args.File.Path)
	} else {
		Log(args.Config, "GenerateRules:", args.Rel, args.File)
	}

	erlangConfig := erlangConfigForRel(args.Config, args.Rel)
	Log(args.Config, "    ", erlangConfig)

	// TODO: check if the generated files would tell us if we don't need to generate
	//
	//       hmm, empty. maybe only works in current dir
	//       no, still empty everwhere but ampq_client with the generated hrl and erl
	//       okay, does not include generated files in subirs
	//       this points to src rules inside an ebin dir, which won't exist by default unfortunately
	//       so it's tricky/impossible to generate the non-test beam in the ebin dir without rules in
	//       the parent, even though that would be nice. Current convention is prod beam in ebin,
	//       test beam in /src. Since the current macro generate in subdirs, I think we are stuck
	//       with needing the directive, even if we generated test beam rules inside the src dir.
	//       Only other option would be to patch the gazelle rule to generate ebin dirs and .gitkeep
	//       them... (unless maybe the Configure steps could do it?)
	// Log(args.Config, "    GenFiles:", args.GenFiles)
	if args.File != nil {
		for _, r := range args.File.Rules {
			if erlangConfig.ExcludeWhenRuleOfKindExists[r.Kind()] {
				Log(args.Config, "    Skipping", args.Rel, "as a", r.Kind(), "is already defined")
				return language.GenerateResult{}
			}
		}
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
	} else if isProbablyBareErlang(args) {
		err := importBareErlang(args, erlangApp)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}
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

	if args.Rel == "" {
		erlc_opts := rule.NewRule("erlc_opts", erlcOptsRuleName)
		erlc_opts.SetAttr("values", erlcOptsWithSelect(erlangApp.ErlcOpts))
		erlc_opts.SetAttr("visibility", []string{":__subpackages__"})

		result.Gen = append(result.Gen, erlc_opts)
		result.Imports = append(result.Imports, erlc_opts.PrivateAttr(config.GazelleImportsKey))

		test_erlc_opts := rule.NewRule("erlc_opts", testErlcOptsRuleName)
		test_erlc_opts.SetAttr("values", erlcOptsWithSelect(erlangApp.TestErlcOpts))
		test_erlc_opts.SetAttr("visibility", []string{":__subpackages__"})

		result.Gen = append(result.Gen, test_erlc_opts)
		result.Imports = append(result.Imports, test_erlc_opts.PrivateAttr(config.GazelleImportsKey))
	}

	if erlangApp.Srcs.Empty() {
		return result
	}

	Log(args.Config, "    Analyzing sources...")

	erlParser := newErlParser(args.Config.RepoRoot, args.Rel)

	var beamFilesRules, testBeamFilesRules []*rule.Rule

	outs := treeset.NewWith(godsutils.StringComparator)
	testOuts := treeset.NewWith(godsutils.StringComparator)
	for _, s := range erlangApp.Srcs.Values() {
		src := s.(string)

		Log(args.Config, "        Parsing", src, "->", filepath.Join(sourcePrefix, src))
		erlAttrs, err := erlParser.parseErl(filepath.Join(sourcePrefix, src))
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}

		var theseHdrs []string
		for _, include := range erlAttrs.Include {
			for _, h := range erlangApp.PrivateHdrs.Union(erlangApp.PublicHdrs).Values() {
				hdr := h.(string)
				if path.Base(hdr) == include {
					theseHdrs = append(theseHdrs, hdr)
				}
			}
		}

		var theseDeps []string
		for _, include := range erlAttrs.IncludeLib {
			parts := strings.Split(include, string(os.PathSeparator))
			if len(parts) > 0 {
				if !erlangConfig.IgnoredDeps[parts[0]] {
					theseDeps = append(theseDeps, parts[0])
				}
			}
		}

		var theseBeam []string
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
				if dep, found := erlangConfig.BehaviourMappings[behaviour]; found {
					theseDeps = append(theseDeps, dep)
				}
			}
		}

		out := beamFile(src)
		outs.Add(out)

		erlang_bytecode := rule.NewRule("erlang_bytecode", ruleName(out))
		erlang_bytecode.SetAttr("srcs", []interface{}{src})
		if len(theseHdrs) > 0 {
			erlang_bytecode.SetAttr("hdrs", theseHdrs)
		}
		erlang_bytecode.SetAttr("erlc_opts", "//:"+erlcOptsRuleName)
		erlang_bytecode.SetAttr("outs", []string{out})
		if len(theseBeam) > 0 {
			erlang_bytecode.SetAttr("beam", theseBeam)
		}
		if len(theseDeps) > 0 {
			erlang_bytecode.SetAttr("deps", theseDeps)
		}

		beamFilesRules = append(beamFilesRules, erlang_bytecode)

		if !erlangApp.TestSrcs.Empty() {
			test_out := testBeamFile(src)
			testOuts.Add(test_out)

			test_erlang_bytecode := rule.NewRule("erlang_bytecode", ruleName(test_out))
			test_erlang_bytecode.SetAttr("srcs", []interface{}{src})
			if len(theseHdrs) > 0 {
				erlang_bytecode.SetAttr("hdrs", theseHdrs)
			}
			test_erlang_bytecode.SetAttr("erlc_opts", "//:"+testErlcOptsRuleName)
			test_erlang_bytecode.SetAttr("outs", []string{test_out})
			if len(theseBeam) > 0 {
				test_erlang_bytecode.SetAttr("beam", theseBeam)
			}
			if len(theseDeps) > 0 {
				test_erlang_bytecode.SetAttr("deps", theseDeps)
			}
			test_erlang_bytecode.SetAttr("testonly", true)

			testBeamFilesRules = append(testBeamFilesRules, test_erlang_bytecode)
		}
	}

	beam_files := rule.NewRule("filegroup", "beam_files")
	beam_files.SetAttr("srcs", outs.Values())
	beamFilesRules = append(beamFilesRules, beam_files)

	var test_beam_files *rule.Rule
	if !erlangApp.TestSrcs.Empty() {
		test_beam_files = rule.NewRule("filegroup", "test_beam_files")
		test_beam_files.SetAttr("srcs", testOuts.Values())
		test_beam_files.SetAttr("testonly", true)
		testBeamFilesRules = append(testBeamFilesRules, test_beam_files)
	}

	if erlangConfig.GenerateBeamFilesMacro {
		Log(args.Config, "    Adding/updating app.bzl")
		appBzlFile := filepath.Join(args.Config.RepoRoot, args.Rel, macroFileName)
		beamFilesMacro, err := rule.LoadMacroFile(appBzlFile, "", beamFilesKind)
		if os.IsNotExist(err) {
			beamFilesMacro, err = rule.EmptyMacroFile(appBzlFile, "", beamFilesKind)
			if err != nil {
				log.Fatalf("ERROR: %v\n", err)
				// return fmt.Errorf("error creating %q: %v", appBzlFile, err)
			}
		} else if err != nil {
			log.Fatalf("ERROR: %v\n", err)
			// return fmt.Errorf("error loading %q: %v", appBzlFile, err)
		}

		updateRules(beamFilesMacro, beamFilesRules, appBzlFile)
		ensureLoad("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode", 0, beamFilesMacro)
		// NOTE: for some reason, LoadMacroFile ignores any "native.filegroup" rules
		//       present in the macro. Therefore, we use our own "alias" of the
		//       macro so that updates to the macro are stable
		ensureLoad("@rules_erlang//:filegroup.bzl", "filegroup", 1, beamFilesMacro)
		beamFilesMacro.Save(appBzlFile)

		beamFilesCall := rule.NewRule(beamFilesKind, "")
		result.Gen = append(result.Gen, beamFilesCall)
		result.Imports = append(result.Imports, beamFilesCall.PrivateAttr(config.GazelleImportsKey))

		if !erlangApp.TestSrcs.Empty() {
			var testBeamFilesMacro *rule.File
			if !erlangApp.TestSrcs.Empty() {
				testBeamFilesMacro, err = rule.LoadMacroFile(appBzlFile, "", testBeamFilesKind)
				if os.IsNotExist(err) {
					testBeamFilesMacro, err = rule.EmptyMacroFile(appBzlFile, "", testBeamFilesKind)
					if err != nil {
						log.Fatalf("ERROR: %v\n", err)
						// return fmt.Errorf("error creating %q: %v", appBzlFile, err)
					}
				} else if err != nil {
					log.Fatalf("ERROR: %v\n", err)
					// return fmt.Errorf("error loading %q: %v", appBzlFile, err)
				}
			}

			updateRules(testBeamFilesMacro, testBeamFilesRules, appBzlFile)
			testBeamFilesMacro.Save(appBzlFile)

			testBeamFilesCall := rule.NewRule(testBeamFilesKind, "")
			result.Gen = append(result.Gen, testBeamFilesCall)
			result.Imports = append(result.Imports, testBeamFilesCall.PrivateAttr(config.GazelleImportsKey))
		}
	} else {
		for i, _ := range beamFilesRules {
			result.Gen = append(result.Gen, beamFilesRules[i])
			result.Imports = append(result.Imports, beamFilesRules[i].PrivateAttr(config.GazelleImportsKey))
			if !erlangApp.TestSrcs.Empty() {
				result.Gen = append(result.Gen, testBeamFilesRules[i])
				result.Imports = append(result.Imports, testBeamFilesRules[i].PrivateAttr(config.GazelleImportsKey))
			}
		}
	}

	// TODO: handle the existence of a static .app file in src/
	app_file := rule.NewRule("app_file", "app_file")
	if erlangApp.Description != "" {
		app_file.SetAttr("app_description", erlangApp.Description)
	}
	app_file.SetAttr("app_name", erlangApp.Name)
	if !erlangApp.AppSrc.Empty() {
		app_file.SetAttr("app_src", erlangApp.AppSrc.Values())
	}
	if erlangApp.Version != "" {
		app_file.SetAttr("app_version", erlangApp.Version)
	}
	app_file.SetAttr("out", filepath.Join("ebin", erlangApp.Name+".app"))
	app_file.SetAttr("modules", []string{":" + beam_files.Name()})
	if !erlangApp.ExtraApps.Empty() {
		app_file.SetAttr("extra_apps", erlangApp.ExtraApps.Values())
	}
	if !erlangApp.Deps.Empty() {
		app_file.SetAttr("deps", erlangApp.Deps.Values())
	}

	result.Gen = append(result.Gen, app_file)
	result.Imports = append(result.Imports, app_file.PrivateAttr(config.GazelleImportsKey))

	erlang_app_info := rule.NewRule("erlang_app_info", "erlang_app")
	erlang_app_info.SetAttr("srcs", erlangApp.Srcs.Union(erlangApp.PrivateHdrs).Union(erlangApp.PublicHdrs).Union(erlangApp.AppSrc).Values())
	erlang_app_info.SetAttr("hdrs", erlangApp.PublicHdrs.Values())
	erlang_app_info.SetAttr("app", ":"+app_file.Name())
	erlang_app_info.SetAttr("app_name", erlangApp.Name)
	erlang_app_info.SetAttr("beam", []string{":" + beam_files.Name()})
	erlang_app_info.SetAttr("license_files", erlangApp.LicenseFiles.Values())
	erlang_app_info.SetAttr("visibility", []string{"//visibility:public"})
	if !erlangApp.Deps.Empty() {
		erlang_app_info.SetAttr("deps", erlangApp.Deps.Values())
	}

	result.Gen = append(result.Gen, erlang_app_info)
	result.Imports = append(result.Imports, erlang_app_info.PrivateAttr(config.GazelleImportsKey))

	if !erlangApp.TestSrcs.Empty() {
		test_erlang_app_info := rule.NewRule("erlang_app_info", "test_erlang_app")
		test_erlang_app_info.SetAttr("srcs", erlangApp.Srcs.Union(erlangApp.PrivateHdrs).Union(erlangApp.PublicHdrs).Union(erlangApp.AppSrc).Values())
		test_erlang_app_info.SetAttr("hdrs", erlangApp.PublicHdrs.Values())
		test_erlang_app_info.SetAttr("app", ":"+app_file.Name())
		test_erlang_app_info.SetAttr("app_name", erlangApp.Name)
		test_erlang_app_info.SetAttr("beam", []string{":" + test_beam_files.Name()})
		test_erlang_app_info.SetAttr("license_files", erlangApp.LicenseFiles.Values())
		test_erlang_app_info.SetAttr("visibility", []string{"//visibility:public"})
		test_erlang_app_info.SetAttr("testonly", true)
		if !erlangApp.Deps.Empty() {
			test_erlang_app_info.SetAttr("deps", erlangApp.Deps.Values())
		}

		result.Gen = append(result.Gen, test_erlang_app_info)
		result.Imports = append(result.Imports, test_erlang_app_info.PrivateAttr(config.GazelleImportsKey))
	}

	alias := rule.NewRule("alias", erlangApp.Name)
	alias.SetAttr("actual", ":erlang_app")
	alias.SetAttr("visibility", []string{"//visibility:public"})

	result.Gen = append(result.Gen, alias)
	result.Imports = append(result.Imports, alias.PrivateAttr(config.GazelleImportsKey))

	return result
}
