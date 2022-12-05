package erlang

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
	"github.com/bazelbuild/buildtools/build"
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
		err := filepath.WalkDir(filepath.Join(args.Config.RepoRoot, args.Rel, "src"),
			func(path string, info os.DirEntry, err error) error {
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

func importHexPmTar(args language.GenerateArgs, result *language.GenerateResult, erlangApp *erlangApp) error {
	Log(args.Config, "    Importing Hex.pm archive from", filepath.Join(args.Config.RepoRoot, args.Rel))

	hexMetadataParser := newHexMetadataParser(args.Config.RepoRoot, args.Rel)

	hexMetadata, err := hexMetadataParser.parseHexMetadata(hexMetadataFilename)
	if err != nil {
		return err
	}

	Log(args.Config, "    hexMetadata:", hexMetadata)

	erlangApp.Name = hexMetadata.Name
	erlangApp.Description = hexMetadata.Description
	erlangApp.Version = hexMetadata.Version

	erlangConfig := erlangConfigForRel(args.Config, args.Rel)

	untar := rule.NewRule("untar", "contents")
	untar.SetAttr("archive", hexContentsArchiveFilename)
	untar.SetAttr("outs", filterOutDirectories(hexMetadata.Files))
	maybeAppendRule(erlangConfig, untar, result)

	for _, f := range hexMetadata.Files {
		erlangApp.addFile(f)
	}

	for _, req := range hexMetadata.Requirements {
		if !req.Optional {
			erlangApp.Deps.Add(req.App)
		}
	}

	// extract to a temporary directory
	extractedContentsDir, err := ioutil.TempDir("", hexMetadata.Name)
	if err != nil {
		return err
	}
	Log(args.Config, "    tempDir:", extractedContentsDir)
	erlangApp.RepoRoot = extractedContentsDir

	hexContentsArchivePath := filepath.Join(args.Config.RepoRoot, args.Rel, hexContentsArchiveFilename)
	Log(args.Config, "    hexContentsArchivePath:", hexContentsArchivePath)
	err = ExtractTarGz(hexContentsArchivePath, extractedContentsDir)
	if err != nil {
		return err
	}

	if Contains(hexMetadata.BuildTools, "rebar3") {
		err = importRebar(args, erlangApp)
		if err != nil {
			return err
		}
	}

	return nil
}

func importRebar(args language.GenerateArgs, erlangApp *erlangApp) error {
	rebarAppPath := filepath.Join(erlangApp.RepoRoot, erlangApp.Rel)
	Log(args.Config, "    Importing Rebar from",
		filepath.Join(rebarAppPath, rebarConfigFilename))

	rebarConfigParser := newRebarConfigParser(erlangApp.RepoRoot, erlangApp.Rel)
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

	if erlangApp.Srcs.IsEmpty() {
		err := filepath.WalkDir(rebarAppPath,
			func(path string, info os.DirEntry, err error) error {
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
				erlangApp.addFile(rel)
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

	err := filepath.WalkDir(appPath,
		func(path string, info os.DirEntry, err error) error {
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
			if rel != filepath.Base(rel) && args.Config.IsValidBuildFileName(info.Name()) {
				return filepath.SkipDir
			}
			erlangApp.addFile(rel)
			return nil
		})
	if err != nil {
		log.Println(err)
	}
	for _, f := range args.GenFiles {
		// Log(args.Config, "        ", f)
		erlangApp.addFile(f)
	}

	return nil
}

func mergeAttr(src, dst *rule.Rule, attr string) {
	srcVals := NewMutableSet(src.AttrStrings(attr)...)
	dstVals := NewMutableSet(dst.AttrStrings(attr)...)
	mergedVals := Union(srcVals, dstVals)
	if mergedVals.IsEmpty() {
		dst.DelAttr(attr)
	} else {
		dst.SetAttr(attr, mergedVals.Values(strings.Compare))
	}
}

func mergeRule(src, dst *rule.Rule) {
	dst.SetName(src.Name())
	dst.SetAttr("srcs", src.AttrStrings("srcs"))
	if dst.Kind() == erlangBytecodeKind {
		dst.SetAttr("outs", src.AttrStrings("outs"))

		if src.Attr("erlc_opts") != nil {
			dst.SetAttr("erlc_opts", src.Attr("erlc_opts"))
		}

		if src.Attr("app_name") != nil {
			dst.SetAttr("app_name", src.Attr("app_name"))
		}

		mergeAttr(src, dst, "hdrs")
		mergeAttr(src, dst, "beam")
		mergeAttr(src, dst, "deps")
	}
}

func updateRules(c *config.Config, f *rule.File, rules []*rule.Rule, filename string) {
	oldToNew := make(map[*rule.Rule]*rule.Rule)
	var strictlyNew []*rule.Rule

	for _, newRule := range rules {
		matched := false
		for _, oldRule := range f.Rules {
			if oldRule.Name() == newRule.Name() {
				oldToNew[oldRule] = newRule
				matched = true
				break
			}
		}
		if !matched {
			strictlyNew = append(strictlyNew, newRule)
		}
	}
	for _, oldRule := range f.Rules {
		if newRule, ok := oldToNew[oldRule]; ok {
			resolveErlangDeps(c, f.Pkg, newRule)
			mergeRule(newRule, oldRule)
		} else {
			oldRule.Delete()
		}
	}
	for _, newRule := range strictlyNew {
		resolveErlangDeps(c, f.Pkg, newRule)
		newRule.Insert(f)
	}
	// TODO: fix the sort to actully do something
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

func macroFile(path string, defName string) (*rule.File, error) {
	macroFile, err := rule.LoadMacroFile(path, "", defName)
	if os.IsNotExist(err) {
		macroFile, err = rule.EmptyMacroFile(path, "", defName)
		if err != nil {
			return nil, fmt.Errorf("error creating %q: %v", path, err)
		}
	} else if err != nil {
		return nil, fmt.Errorf("error loading %q: %v", path, err)
	}
	return macroFile, nil
}

func addNameArg(macroFile *rule.File) {
	for _, s := range macroFile.File.Stmt {
		if defStmt, ok := s.(*build.DefStmt); ok {
			if defStmt.Name == macroFile.DefName {
				nameAttr := &build.AssignExpr{
					LHS: &build.Ident{Name: "name"},
					RHS: &build.StringExpr{Value: macroFile.DefName},
					Op:  "=",
				}
				defStmt.Params = []build.Expr{nameAttr}
			}
		}
	}
}

func maybeAppendRule(erlangConfig *ErlangConfig, rule *rule.Rule, result *language.GenerateResult) {
	if !erlangConfig.GenerateSkipRules.Contains(rule.Kind()) {
		result.Gen = append(result.Gen, rule)
		result.Imports = append(result.Imports, rule.PrivateAttr(config.GazelleImportsKey))
	}
}

func (erlang *erlangLang) GenerateRules(args language.GenerateArgs) language.GenerateResult {
	if args.File != nil {
		Log(args.Config, "GenerateRules:", args.Rel, args.File.Path)
	} else {
		Log(args.Config, "GenerateRules:", args.Rel, args.File)
	}

	erlangConfig := erlangConfigForRel(args.Config, args.Rel)
	// Log(args.Config, "    ", erlangConfig)

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

	erlangApp := newErlangApp(args.Config.RepoRoot, args.Rel)
	for dep := range erlangConfig.Deps {
		erlangApp.Deps.Add(dep)
	}

	for app := range erlangConfig.ExtraApps {
		erlangApp.ExtraApps.Add(app)
	}

	if isHexPmTar(args.RegularFiles) {
		err := importHexPmTar(args, &result, erlangApp)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}
		defer os.RemoveAll(erlangApp.RepoRoot)
	} else if isRebar(args.RegularFiles) {
		err := importRebar(args, erlangApp)
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
		if !erlangApp.AppSrc.IsEmpty() {
			dotAppParser := newDotAppParser(args.Config.RepoRoot, args.Rel)
			dotApp, err := dotAppParser.parseAppSrc(erlangApp.AppSrc.Any())
			if err != nil {
				log.Fatalf("ERROR: %v\n", err)
			}

			erlangApp.Name = strings.TrimSuffix(filepath.Base(erlangApp.AppSrc.Any()), ".app.src")
			// TODO handle when the filename does not match the contents
			props := (*dotApp)[erlangApp.Name]
			for _, app := range props.Applications {
				if !Contains([]string{"kernel", "stdlib"}, app) {
					erlangApp.ExtraApps.Add(app)
				}
			}
			erlangApp.ExtraApps.Subtract(erlangApp.Deps)
		} else {
			erlangApp.Name = filepath.Base(filepath.Join(
				erlangApp.RepoRoot, erlangApp.Rel))
		}
	}

	if args.Rel == "" {
		erlc_opts := erlangApp.erlcOptsRule()
		maybeAppendRule(erlangConfig, erlc_opts, &result)

		test_erlc_opts := erlangApp.testErlcOptsRule()
		maybeAppendRule(erlangConfig, test_erlc_opts, &result)

		basePltRule := erlangApp.basePltRule()
		maybeAppendRule(erlangConfig, basePltRule, &result)
	}

	if erlangApp.Srcs.IsEmpty() {
		return result
	}

	Log(args.Config, "    Analyzing sources...")

	erlParser := newErlParser()

	beamFilesRules, testBeamFilesRules := erlangApp.beamFilesRules(args, erlParser)

	allSrcsRules := erlangApp.allSrcsRules()

	testDirBeamFilesRules := erlangApp.testDirBeamFilesRules(args, erlParser)

	if erlangConfig.GenerateBeamFilesMacro {
		Log(args.Config, "    Adding/updating app.bzl")
		appBzlFile := filepath.Join(args.Config.RepoRoot, args.Rel, macroFileName)
		beamFilesMacro, err := macroFile(appBzlFile, allBeamFilesKind)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}

		updateRules(args.Config, beamFilesMacro, beamFilesRules, appBzlFile)
		ensureLoad("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode", 0, beamFilesMacro)
		// NOTE: for some reason, LoadMacroFile ignores any "native.filegroup" rules
		//       present in the macro. Therefore, we use our own "alias" of the
		//       macro so that updates to the macro are stable
		ensureLoad("@rules_erlang//:filegroup.bzl", "filegroup", 1, beamFilesMacro)
		addNameArg(beamFilesMacro)
		beamFilesMacro.Save(appBzlFile)

		beamFilesCall := rule.NewRule(allBeamFilesKind, allBeamFilesKind)
		maybeAppendRule(erlangConfig, beamFilesCall, &result)

		testBeamFilesMacro, err := macroFile(appBzlFile, allTestBeamFilesKind)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}

		updateRules(args.Config, testBeamFilesMacro, testBeamFilesRules, appBzlFile)
		addNameArg(testBeamFilesMacro)
		testBeamFilesMacro.Save(appBzlFile)

		testDirBeamFilesMacro, err := macroFile(appBzlFile, testSuiteBeamFilesKind)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}

		updateRules(args.Config, testDirBeamFilesMacro, testDirBeamFilesRules, appBzlFile)
		addNameArg(testDirBeamFilesMacro)
		testDirBeamFilesMacro.Save(appBzlFile)

		if erlangApp.hasTestSuites() || erlangConfig.GenerateTestBeamUnconditionally {
			testBeamFilesCall := rule.NewRule(allTestBeamFilesKind, allTestBeamFilesKind)
			maybeAppendRule(erlangConfig, testBeamFilesCall, &result)

			testSuitesBeamFilesCall := rule.NewRule(testSuiteBeamFilesKind, testSuiteBeamFilesKind)
			maybeAppendRule(erlangConfig, testSuitesBeamFilesCall, &result)
		}

		allSrcsMacro, err := macroFile(appBzlFile, allSrcsKind)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}

		updateRules(args.Config, allSrcsMacro, allSrcsRules, appBzlFile)
		addNameArg(allSrcsMacro)
		allSrcsMacro.Save(appBzlFile)

		allSrcsCall := rule.NewRule(allSrcsKind, allSrcsKind)
		maybeAppendRule(erlangConfig, allSrcsCall, &result)
	} else {
		for i := range beamFilesRules {
			maybeAppendRule(erlangConfig, beamFilesRules[i], &result)
			if erlangApp.hasTestSuites() || erlangConfig.GenerateTestBeamUnconditionally {
				maybeAppendRule(erlangConfig, testBeamFilesRules[i], &result)
			}
		}
		for _, r := range testDirBeamFilesRules {
			maybeAppendRule(erlangConfig, r, &result)
		}
		for _, r := range allSrcsRules {
			maybeAppendRule(erlangConfig, r, &result)
		}
	}

	explicitFiles := erlangApp.RepoRoot != args.Config.RepoRoot
	erlang_app := erlangApp.erlangAppRule(explicitFiles)
	maybeAppendRule(erlangConfig, erlang_app, &result)

	if erlangApp.hasTestSuites() || erlangConfig.GenerateTestBeamUnconditionally {
		test_erlang_app := erlangApp.testErlangAppRule(explicitFiles)
		maybeAppendRule(erlangConfig, test_erlang_app, &result)
	}

	alias := rule.NewRule("alias", erlangApp.Name)
	alias.SetAttr("actual", ":erlang_app")
	alias.SetAttr("visibility", []string{"//visibility:public"})
	maybeAppendRule(erlangConfig, alias, &result)

	xrefRule := erlangApp.xrefRule()
	maybeAppendRule(erlangConfig, xrefRule, &result)

	pltRule := erlangApp.appPltRule()
	maybeAppendRule(erlangConfig, pltRule, &result)

	dialyzeRule := erlangApp.dialyzeRule()
	maybeAppendRule(erlangConfig, dialyzeRule, &result)

	if erlangApp.hasTestSuites() {
		eunitRule := erlangApp.eunitRule()
		maybeAppendRule(erlangConfig, eunitRule, &result)

		ctSuiteRules := erlangApp.ctSuiteRules()
		for _, r := range ctSuiteRules {
			maybeAppendRule(erlangConfig, r, &result)
		}
	}

	assert_suites := rule.NewRule(assertSuitesKind, "")
	maybeAppendRule(erlangConfig, assert_suites, &result)

	// Log(args.Config, "    result.Gen", Map(func(r *rule.Rule) string {
	// 	return r.Kind() + "/" + r.Name()
	// }, result.Gen))

	return result
}
