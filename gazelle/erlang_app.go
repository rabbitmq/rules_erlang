package erlang

import (
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

type erlangApp struct {
	Name         string
	Description  string
	Version      string
	Srcs         MutableSet[string]
	PrivateHdrs  MutableSet[string]
	PublicHdrs   MutableSet[string]
	AppSrc       MutableSet[string]
	TestSrcs     MutableSet[string]
	TestHdrs     MutableSet[string]
	LicenseFiles MutableSet[string]
	ErlcOpts     []string
	TestErlcOpts []string
	Deps         MutableSet[string]
	ExtraApps    MutableSet[string]
}

func newErlangApp() *erlangApp {
	return &erlangApp{
		Srcs:         NewMutableSet[string](),
		PrivateHdrs:  NewMutableSet[string](),
		PublicHdrs:   NewMutableSet[string](),
		AppSrc:       NewMutableSet[string](),
		TestSrcs:     NewMutableSet[string](),
		TestHdrs:     NewMutableSet[string](),
		LicenseFiles: NewMutableSet[string](),
		ErlcOpts:     []string{"+debug_info"},
		TestErlcOpts: []string{"+debug_info", "-DTEST=1"},
		Deps:         NewMutableSet[string](),
		ExtraApps:    NewMutableSet[string](),
	}
}

func (erlangApp *erlangApp) addFile(f string) {
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
	base := filepath.Base(src)
	if strings.HasSuffix(base, ".erl") {
		return strings.TrimSuffix(base, ".erl")
	}
	return strings.TrimSuffix(base, ".beam")
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

func (erlangApp *erlangApp) pathFor(include string) string {
	privatePath := filepath.Join("src", include)
	if erlangApp.PrivateHdrs.Contains(privatePath) {
		return privatePath
	}
	publicPath := filepath.Join("include", include)
	if erlangApp.PublicHdrs.Contains(publicPath) {
		return publicPath
	}
	return ""
}

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

func (erlangApp *erlangApp) erlcOptsRule() *rule.Rule {
	erlc_opts := rule.NewRule("erlc_opts", erlcOptsRuleName)
	erlc_opts.SetAttr("values", erlcOptsWithSelect(erlangApp.ErlcOpts))
	erlc_opts.SetAttr("visibility", []string{":__subpackages__"})
	return erlc_opts
}

func (erlangApp *erlangApp) testErlcOptsRule() *rule.Rule {
	test_erlc_opts := rule.NewRule("erlc_opts", testErlcOptsRuleName)
	test_erlc_opts.SetAttr("values", erlcOptsWithSelect(erlangApp.TestErlcOpts))
	test_erlc_opts.SetAttr("visibility", []string{":__subpackages__"})
	return test_erlc_opts
}

func (erlangApp *erlangApp) beamFilesRules(args language.GenerateArgs, erlParser *erlParser, sourcePrefix string) (beamFilesRules, testBeamFilesRules []*rule.Rule) {
	erlangConfig := erlangConfigForRel(args.Config, args.Rel)

	outs := NewMutableSet[string]()
	testOuts := NewMutableSet[string]()
	for _, src := range erlangApp.Srcs.Values(strings.Compare) {
		actualPath := filepath.Join(sourcePrefix, src)
		// TODO: not print Parsing when the file does not exist
		Log(args.Config, "        Parsing", src, "->", actualPath)
		erlAttrs, err := erlParser.deepParseErl(actualPath, erlangApp)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}

		theseHdrs := NewMutableSet[string]()
		for _, include := range erlAttrs.Include {
			path := erlangApp.pathFor(include)
			if path != "" {
				Log(args.Config, "            include", path)
				theseHdrs.Add(path)
			}
		}

		theseDeps := NewMutableSet[string]()
		for _, include := range erlAttrs.IncludeLib {
			parts := strings.Split(include, string(os.PathSeparator))
			if len(parts) > 0 {
				if !erlangConfig.IgnoredDeps[parts[0]] {
					Log(args.Config, "            include_lib", include, "->", parts[0])
					theseDeps.Add(parts[0])
				}
			}
		}

		theseBeam := NewMutableSet[string]()
		for _, behaviour := range erlAttrs.Behaviour {
			found := false
			for _, src := range erlangApp.Srcs.Values(strings.Compare) {
				if moduleName(src) == behaviour {
					Log(args.Config, "            behaviour", behaviour, "->", beamFile(src))
					theseBeam.Add(beamFile(src))
					found = true
					break
				}
			}
			if !found {
				if dep, found := erlangConfig.BehaviourMappings[behaviour]; found {
					Log(args.Config, "            behaviour", behaviour, "->", dep)
					theseDeps.Add(dep)
				}
			}
		}

		out := beamFile(src)
		outs.Add(out)

		erlang_bytecode := rule.NewRule(erlangBytecodeKind, ruleName(out))
		erlang_bytecode.SetAttr("srcs", []interface{}{src})
		if !theseHdrs.IsEmpty() {
			erlang_bytecode.SetAttr("hdrs", theseHdrs.Values(strings.Compare))
		}
		erlang_bytecode.SetAttr("erlc_opts", "//:"+erlcOptsRuleName)
		erlang_bytecode.SetAttr("outs", []string{out})
		if !theseBeam.IsEmpty() {
			erlang_bytecode.SetAttr("beam", theseBeam.Values(strings.Compare))
		}
		if !theseDeps.IsEmpty() {
			erlang_bytecode.SetAttr("deps", theseDeps.Values(strings.Compare))
		}

		beamFilesRules = append(beamFilesRules, erlang_bytecode)

		test_out := testBeamFile(src)
		testOuts.Add(test_out)

		test_erlang_bytecode := rule.NewRule(erlangBytecodeKind, ruleName(test_out))
		test_erlang_bytecode.SetAttr("srcs", []interface{}{src})
		if !theseHdrs.IsEmpty() {
			test_erlang_bytecode.SetAttr("hdrs", theseHdrs.Values(strings.Compare))
		}
		test_erlang_bytecode.SetAttr("erlc_opts", "//:"+testErlcOptsRuleName)
		test_erlang_bytecode.SetAttr("outs", []string{test_out})
		if !theseBeam.IsEmpty() {
			test_erlang_bytecode.SetAttr("beam", theseBeam.Values(strings.Compare))
		}
		if !theseDeps.IsEmpty() {
			test_erlang_bytecode.SetAttr("deps", theseDeps.Values(strings.Compare))
		}
		test_erlang_bytecode.SetAttr("testonly", true)

		testBeamFilesRules = append(testBeamFilesRules, test_erlang_bytecode)
	}

	beam_files := rule.NewRule("filegroup", "beam_files")
	beam_files.SetAttr("srcs", outs.Values(strings.Compare))
	beamFilesRules = append(beamFilesRules, beam_files)

	test_beam_files := rule.NewRule("filegroup", "test_beam_files")
	test_beam_files.SetAttr("srcs", testOuts.Values(strings.Compare))
	test_beam_files.SetAttr("testonly", true)
	testBeamFilesRules = append(testBeamFilesRules, test_beam_files)
	return
}

func (erlangApp *erlangApp) allSrcsRule() *rule.Rule {
	all_srcs := rule.NewRule("filegroup", "all_srcs")
	all_srcs.SetAttr("srcs", Union(erlangApp.Srcs, erlangApp.PrivateHdrs, erlangApp.PublicHdrs, erlangApp.AppSrc).Values(strings.Compare))
	return all_srcs
}

func (erlangApp *erlangApp) erlangAppRule(explicitFiles bool) *rule.Rule {
	r := rule.NewRule(erlangAppKind, "")
	r.SetAttr("app_name", erlangApp.Name)
	if erlangApp.Version != "" {
		r.SetAttr("app_version", erlangApp.Version)
	}
	if erlangApp.Description != "" {
		r.SetAttr("app_description", erlangApp.Description)
	}
	if !erlangApp.ExtraApps.IsEmpty() {
		r.SetAttr("extra_apps", erlangApp.ExtraApps.Values(strings.Compare))
	}

	r.SetAttr("beam_files", []string{":beam_files"})
	r.SetAttr("public_hdrs", erlangApp.PublicHdrs.Values(strings.Compare))
	r.SetAttr("all_srcs", []string{":all_srcs"})

	if explicitFiles && !erlangApp.LicenseFiles.IsEmpty() {
		r.SetAttr("extra_license_files", erlangApp.LicenseFiles.Values(strings.Compare))
	}

	if !erlangApp.Deps.IsEmpty() {
		r.SetAttr("deps", erlangApp.Deps.Values(strings.Compare))
	}
	return r
}

func (erlangApp *erlangApp) testErlangAppRule(explicitFiles bool) *rule.Rule {
	r := rule.NewRule(testErlangAppKind, "")
	r.SetAttr("app_name", erlangApp.Name)
	if erlangApp.Version != "" {
		r.SetAttr("app_version", erlangApp.Version)
	}
	if erlangApp.Description != "" {
		r.SetAttr("app_description", erlangApp.Description)
	}
	if !erlangApp.ExtraApps.IsEmpty() {
		r.SetAttr("extra_apps", erlangApp.ExtraApps.Values(strings.Compare))
	}

	r.SetAttr("beam_files", []string{":test_beam_files"})
	r.SetAttr("public_hdrs", Union(erlangApp.PublicHdrs, erlangApp.PrivateHdrs).Values(strings.Compare))
	r.SetAttr("all_srcs", []string{":all_srcs"})

	if explicitFiles && !erlangApp.LicenseFiles.IsEmpty() {
		r.SetAttr("extra_license_files", erlangApp.LicenseFiles.Values(strings.Compare))
	}

	if !erlangApp.Deps.IsEmpty() {
		r.SetAttr("deps", erlangApp.Deps.Values(strings.Compare))
	}
	return r
}

func ruleNameForTestSrc(f string) string {
	modName := moduleName(f)
	if strings.HasSuffix(modName, "_SUITE") {
		return modName + "_beam_files"
	} else {
		return ruleName(f)
	}
}

func (erlangApp *erlangApp) testPathFor(include string) string {
	standardPath := erlangApp.pathFor(include)
	if standardPath != "" {
		return standardPath
	}
	testPath := filepath.Join("test", include)
	if erlangApp.PrivateHdrs.Contains(testPath) {
		return testPath
	}
	return ""
}

func (erlangApp *erlangApp) testDirBeamFilesRules(args language.GenerateArgs, erlParser *erlParser, sourcePrefix string) []*rule.Rule {
	erlangConfig := erlangConfigForRel(args.Config, args.Rel)

	var beamFilesRules []*rule.Rule
	outs := NewMutableSet[string]()
	for _, src := range erlangApp.TestSrcs.Values(strings.Compare) {
		actualPath := filepath.Join(sourcePrefix, src)
		Log(args.Config, "        Parsing", src, "->", actualPath)
		erlAttrs, err := erlParser.deepParseErl(actualPath, erlangApp)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}

		theseHdrs := NewMutableSet[string]()
		for _, include := range erlAttrs.Include {
			path := erlangApp.testPathFor(include)
			if path != "" {
				Log(args.Config, "            include", path)
				theseHdrs.Add(path)
			}
		}

		theseDeps := NewMutableSet[string]()
		for _, include := range erlAttrs.IncludeLib {
			parts := strings.Split(include, string(os.PathSeparator))
			if len(parts) > 0 {
				if !erlangConfig.IgnoredDeps[parts[0]] {
					Log(args.Config, "            include_lib", include, "->", parts[0])
					theseDeps.Add(parts[0])
				}
			}
		}

		theseBeam := NewMutableSet[string]()
		for _, behaviour := range erlAttrs.Behaviour {
			found := false
			for _, src := range erlangApp.Srcs.Values(strings.Compare) {
				if moduleName(src) == behaviour {
					Log(args.Config, "            behaviour", behaviour, "->", beamFile(src))
					theseBeam.Add(beamFile(src))
					found = true
					break
				}
			}
			if !found {
				if dep, found := erlangConfig.BehaviourMappings[behaviour]; found {
					Log(args.Config, "            behaviour", behaviour, "->", dep)
					theseDeps.Add(dep)
				}
			}
		}

		out := strings.TrimSuffix(src, ".erl") + ".beam"
		outs.Add(out)

		erlang_bytecode := rule.NewRule(erlangBytecodeKind, ruleNameForTestSrc(out))
		erlang_bytecode.SetAttr("srcs", []interface{}{src})
		if !theseHdrs.IsEmpty() {
			erlang_bytecode.SetAttr("hdrs", theseHdrs.Values(strings.Compare))
		}
		erlang_bytecode.SetAttr("erlc_opts", "//:"+testErlcOptsRuleName)
		erlang_bytecode.SetAttr("outs", []string{out})
		if !theseBeam.IsEmpty() {
			erlang_bytecode.SetAttr("beam", theseBeam.Values(strings.Compare))
		}
		if !theseDeps.IsEmpty() {
			erlang_bytecode.SetAttr("deps", theseDeps.Values(strings.Compare))
		}
		erlang_bytecode.SetAttr("testonly", true)

		beamFilesRules = append(beamFilesRules, erlang_bytecode)
	}

	return beamFilesRules
}

func (erlangApp *erlangApp) eunitRule() *rule.Rule {
	// eunit_mods is the list of source modules, plus any test module which is
	// not among the source modules with a "_tests" suffix appended
	modMap := make(map[string]string)
	for src := range erlangApp.Srcs {
		modMap[moduleName(src)] = ""
	}
	for testSrc := range erlangApp.TestSrcs {
		tm := moduleName(testSrc)
		if !strings.HasSuffix(tm, "_SUITE") {
			label := ":" + ruleNameForTestSrc(strings.TrimSuffix(testSrc, ".erl")+".beam")
			if !strings.HasSuffix(tm, "_tests") {
				modMap[tm] = label
			}
			if _, present := modMap[strings.TrimSuffix(tm, "_tests")]; !present {
				modMap[tm] = label
			}
		}
	}

	eunit_mods := NewMutableSet[string]()
	compiled_suites := NewMutableSet[string]()
	for mod, beam := range modMap {
		eunit_mods.Add(mod)
		if beam != "" {
			compiled_suites.Add(beam)
		}
	}

	eunit := rule.NewRule(eunitKind, "eunit")
	eunit.SetAttr("compiled_suites", compiled_suites.Values(strings.Compare))
	eunit.SetAttr("eunit_mods", eunit_mods.Values(strings.Compare))
	eunit.SetAttr("deps", []string{":test_erlang_app"})

	return eunit
}

func (erlangApp *erlangApp) ctSuiteRules() []*rule.Rule {
	var rules []*rule.Rule

	for _, testSrc := range erlangApp.TestSrcs.Values(strings.Compare) {
		modName := moduleName(testSrc)
		if strings.HasSuffix(modName, "_SUITE") {
			r := rule.NewRule(ctTestKind, modName)
			r.SetAttr("compiled_suites", []string{":" + modName + "_beam_files"})
			r.SetAttr("data", rule.GlobValue{
				Patterns: []string{"test/" + modName + "_data/**/*"},
			})
			r.SetAttr("deps", []string{":test_erlang_app"})

			rules = append(rules, r)
		}
	}

	return rules
}

func (erlangApp *erlangApp) hasTestSuites() bool {
	return !erlangApp.TestSrcs.IsEmpty()
}
