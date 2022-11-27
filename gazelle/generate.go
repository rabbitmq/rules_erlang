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

	erlangConfig := erlangConfigForRel(args.Config, args.Rel)
	if !erlangConfig.GenerateSkipRules.Contains(untar.Kind()) {
		result.Gen = append(result.Gen, untar)
		result.Imports = append(result.Imports, untar.PrivateAttr(config.GazelleImportsKey))
	}

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
		newSrcs := newRule.AttrStrings("srcs")
		for _, oldRule := range f.Rules {
			oldSrcs := oldRule.AttrStrings("srcs")
			if oldRule.Name() == newRule.Name() || cmp.Equal(oldSrcs, newSrcs) {
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
		if !erlangApp.AppSrc.IsEmpty() {
			dotAppParser := newDotAppParser(args.Config.RepoRoot, args.Rel)
			dotApp, err := dotAppParser.parseAppSrc(erlangApp.AppSrc.Any())
			if err != nil {
				log.Fatalf("ERROR: %v\n", err)
			}

			erlangApp.Name = strings.TrimSuffix(filepath.Base(erlangApp.AppSrc.Any()), ".app.src")
			props := (*dotApp)[erlangApp.Name]
			for _, app := range props.Applications {
				if !Contains([]string{"kernel", "stdlib"}, app) {
					erlangApp.ExtraApps.Add(app)
				}
			}
			erlangApp.ExtraApps.Subtract(erlangApp.Deps)
		} else {
			erlangApp.Name = filepath.Base(sourcePrefix)
		}
	}

	if args.Rel == "" {
		erlc_opts := erlangApp.erlcOptsRule()
		if !erlangConfig.GenerateSkipRules.Contains(erlc_opts.Kind()) {
			result.Gen = append(result.Gen, erlc_opts)
			result.Imports = append(result.Imports, erlc_opts.PrivateAttr(config.GazelleImportsKey))
		}

		test_erlc_opts := erlangApp.testErlcOptsRule()
		if !erlangConfig.GenerateSkipRules.Contains(test_erlc_opts.Kind()) {
			result.Gen = append(result.Gen, test_erlc_opts)
			result.Imports = append(result.Imports, test_erlc_opts.PrivateAttr(config.GazelleImportsKey))
		}
	}

	if erlangApp.Srcs.IsEmpty() {
		return result
	}

	Log(args.Config, "    Analyzing sources...")

	erlParser := newErlParser(args.Config.RepoRoot, args.Rel)

	beamFilesRules, testBeamFilesRules := erlangApp.beamFilesRules(args, erlParser, sourcePrefix)

	all_srcs := erlangApp.allSrcsRule()

	testDirBeamFilesRules := erlangApp.testDirBeamFilesRules(args, erlParser, sourcePrefix)

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
		if !erlangConfig.GenerateSkipRules.Contains(beamFilesCall.Kind()) {
			result.Gen = append(result.Gen, beamFilesCall)
			result.Imports = append(result.Imports, beamFilesCall.PrivateAttr(config.GazelleImportsKey))
		}

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

		if erlangApp.hasTestSuites() {
			testBeamFilesCall := rule.NewRule(allTestBeamFilesKind, allTestBeamFilesKind)
			if !erlangConfig.GenerateSkipRules.Contains(testBeamFilesCall.Kind()) {
				result.Gen = append(result.Gen, testBeamFilesCall)
				result.Imports = append(result.Imports, testBeamFilesCall.PrivateAttr(config.GazelleImportsKey))
			}

			testSuitesBeamFilesCall := rule.NewRule(testSuiteBeamFilesKind, testSuiteBeamFilesKind)
			if !erlangConfig.GenerateSkipRules.Contains(testSuitesBeamFilesCall.Kind()) {
				result.Gen = append(result.Gen, testSuitesBeamFilesCall)
				result.Imports = append(result.Imports, testSuitesBeamFilesCall.PrivateAttr(config.GazelleImportsKey))
			}
		}

		allSrcsMacro, err := macroFile(appBzlFile, allSrcsKind)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}

		updateRules(args.Config, allSrcsMacro, []*rule.Rule{all_srcs}, appBzlFile)
		addNameArg(allSrcsMacro)
		allSrcsMacro.Save(appBzlFile)

		allSrcsCall := rule.NewRule(allSrcsKind, allSrcsKind)
		if !erlangConfig.GenerateSkipRules.Contains(allSrcsCall.Kind()) {
			result.Gen = append(result.Gen, allSrcsCall)
			result.Imports = append(result.Imports, allSrcsCall.PrivateAttr(config.GazelleImportsKey))
		}
	} else {
		for i := range beamFilesRules {
			if !erlangConfig.GenerateSkipRules.Contains(beamFilesRules[i].Kind()) {
				result.Gen = append(result.Gen, beamFilesRules[i])
				result.Imports = append(result.Imports, beamFilesRules[i].PrivateAttr(config.GazelleImportsKey))
			}
			if erlangApp.hasTestSuites() && !erlangConfig.GenerateSkipRules.Contains(testBeamFilesRules[i].Kind()) {
				result.Gen = append(result.Gen, testBeamFilesRules[i])
				result.Imports = append(result.Imports, testBeamFilesRules[i].PrivateAttr(config.GazelleImportsKey))
			}
		}
		for _, r := range testDirBeamFilesRules {
			if !erlangConfig.GenerateSkipRules.Contains(r.Kind()) {
				result.Gen = append(result.Gen, r)
				result.Imports = append(result.Imports, r.PrivateAttr(config.GazelleImportsKey))
			}
		}
		if !erlangConfig.GenerateSkipRules.Contains(all_srcs.Kind()) {
			result.Gen = append(result.Gen, all_srcs)
			result.Imports = append(result.Imports, all_srcs.PrivateAttr(config.GazelleImportsKey))
		}
	}

	explicitFiles := sourcePrefix != filepath.Join(args.Config.RepoRoot, args.Rel)
	erlang_app := erlangApp.erlangAppRule(explicitFiles)
	if !erlangConfig.GenerateSkipRules.Contains(erlang_app.Kind()) {
		result.Gen = append(result.Gen, erlang_app)
		result.Imports = append(result.Imports, erlang_app.PrivateAttr(config.GazelleImportsKey))
	}

	if erlangApp.hasTestSuites() {
		test_erlang_app := erlangApp.testErlangAppRule(explicitFiles)
		if !erlangConfig.GenerateSkipRules.Contains(test_erlang_app.Kind()) {
			result.Gen = append(result.Gen, test_erlang_app)
			result.Imports = append(result.Imports, test_erlang_app.PrivateAttr(config.GazelleImportsKey))
		}
	}

	alias := rule.NewRule("alias", erlangApp.Name)
	alias.SetAttr("actual", ":erlang_app")
	alias.SetAttr("visibility", []string{"//visibility:public"})

	if !erlangConfig.GenerateSkipRules.Contains(alias.Kind()) {
		result.Gen = append(result.Gen, alias)
		result.Imports = append(result.Imports, alias.PrivateAttr(config.GazelleImportsKey))
	}

	if erlangApp.hasTestSuites() {
		eunitRule := erlangApp.eunitRule()
		if !erlangConfig.GenerateSkipRules.Contains(eunitRule.Kind()) {
			result.Gen = append(result.Gen, eunitRule)
			result.Imports = append(result.Imports, eunitRule.PrivateAttr(config.GazelleImportsKey))
		}

		ctSuiteRules := erlangApp.ctSuiteRules()
		for _, r := range ctSuiteRules {
			if !erlangConfig.GenerateSkipRules.Contains(r.Kind()) {
				result.Gen = append(result.Gen, r)
				result.Imports = append(result.Imports, r.PrivateAttr(config.GazelleImportsKey))
			}
		}
	}

	assert_suites := rule.NewRule(assertSuitesKind, "")
	if !erlangConfig.GenerateSkipRules.Contains(assert_suites.Kind()) {
		result.Gen = append(result.Gen, assert_suites)
		result.Imports = append(result.Imports, assert_suites.PrivateAttr(config.GazelleImportsKey))
	}

	// Log(args.Config, "    result.Gen", Map(func(r *rule.Rule) string {
	// 	return r.Kind() + "/" + r.Name()
	// }, result.Gen))

	return result
}
