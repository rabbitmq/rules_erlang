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
	RepoRoot     string
	Rel          string
	Name         string
	Description  string
	Version      string
	Srcs         MutableSet[string]
	PrivateHdrs  MutableSet[string]
	PublicHdrs   MutableSet[string]
	AppSrc       MutableSet[string]
	TestSrcs     MutableSet[string]
	TestHdrs     MutableSet[string]
	Priv         MutableSet[string]
	LicenseFiles MutableSet[string]
	ErlcOpts     []string
	TestErlcOpts []string
	Deps         MutableSet[string]
	ExtraApps    MutableSet[string]
}

func newErlangApp(repoRoot, rel string) *erlangApp {
	return &erlangApp{
		RepoRoot:     repoRoot,
		Rel:          rel,
		Srcs:         NewMutableSet[string](),
		PrivateHdrs:  NewMutableSet[string](),
		PublicHdrs:   NewMutableSet[string](),
		AppSrc:       NewMutableSet[string](),
		TestSrcs:     NewMutableSet[string](),
		TestHdrs:     NewMutableSet[string](),
		Priv:         NewMutableSet[string](),
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
	} else if strings.HasPrefix(f, "priv/") {
		erlangApp.Priv.Add(f)
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

func (erlangApp *erlangApp) pathFor(from, include string) string {
	if erlangApp.PrivateHdrs.Contains(include) || erlangApp.PublicHdrs.Contains(include) {
		return include
	}

	directPath := filepath.Join(filepath.Dir(from), include)
	if erlangApp.PrivateHdrs.Contains(directPath) || erlangApp.PublicHdrs.Contains(directPath) {
		return directPath
	}

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
	erlc_opts := rule.NewRule(erlcOptsKind, erlcOptsRuleName)
	erlc_opts.SetAttr("values", erlcOptsWithSelect(erlangApp.ErlcOpts))
	erlc_opts.SetAttr("visibility", []string{":__subpackages__"})
	return erlc_opts
}

func (erlangApp *erlangApp) testErlcOptsRule() *rule.Rule {
	test_erlc_opts := rule.NewRule(erlcOptsKind, testErlcOptsRuleName)
	test_erlc_opts.SetAttr("values", erlcOptsWithSelect(erlangApp.TestErlcOpts))
	test_erlc_opts.SetAttr("visibility", []string{":__subpackages__"})
	return test_erlc_opts
}

func (erlangApp *erlangApp) basePltRule() *rule.Rule {
	plt := rule.NewRule(pltKind, "base_plt")
	plt.SetAttr("visibility", []string{":__subpackages__"})
	return plt
}

func (erlangApp *erlangApp) beamFilesRules(args language.GenerateArgs, erlParser *erlParser) (beamFilesRules, testBeamFilesRules []*rule.Rule) {
	erlangConfig := erlangConfigForRel(args.Config, args.Rel)

	outs := NewMutableSet[string]()
	testOuts := NewMutableSet[string]()
	for _, src := range erlangApp.Srcs.Values(strings.Compare) {
		actualPath := filepath.Join(erlangApp.RepoRoot, erlangApp.Rel, src)
		// TODO: not print Parsing when the file does not exist
		Log(args.Config, "        Parsing", src, "->", actualPath)
		erlAttrs, err := erlParser.deepParseErl(src, erlangApp)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}

		theseHdrs := NewMutableSet[string]()
		for _, include := range erlAttrs.Include {
			path := erlangApp.pathFor(src, include)
			if path != "" {
				Log(args.Config, "            include", path)
				theseHdrs.Add(path)
			} else {
				Log(args.Config, "            ignoring include",
					include, "as it cannot be found")
			}
		}

		theseDeps := NewMutableSet[string]()
		for _, include := range erlAttrs.IncludeLib {
			parts := strings.Split(include, string(os.PathSeparator))
			if len(parts) > 0 {
				if parts[0] == erlangApp.Name {
					path := erlangApp.pathFor(src, strings.Join(parts[1:], string(os.PathSeparator)))
					if path != "" {
						Log(args.Config, "            include_lib (self)", path)
						theseHdrs.Add(path)
					} else {
						Log(args.Config, "            ignoring include_lib (self)",
							include, "as it cannot be found")
					}
				} else if !erlangConfig.IgnoredDeps[parts[0]] {
					Log(args.Config, "            include_lib", include, "->", parts[0])
					theseDeps.Add(parts[0])
				} else {
					Log(args.Config, "            ignoring include_lib", include)
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
		erlang_bytecode.SetAttr("app_name", erlangApp.Name)
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
		test_erlang_bytecode.SetAttr("app_name", erlangApp.Name)
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

func (erlangApp *erlangApp) allSrcsRules() []*rule.Rule {
	var rules []*rule.Rule

	srcs := rule.NewRule("filegroup", "srcs")
	srcs.SetAttr("srcs", Union(erlangApp.Srcs, erlangApp.AppSrc).Values(strings.Compare))
	rules = append(rules, srcs)

	private_hdrs := rule.NewRule("filegroup", "private_hdrs")
	private_hdrs.SetAttr("srcs", erlangApp.PrivateHdrs.Values(strings.Compare))
	rules = append(rules, private_hdrs)

	public_hdrs := rule.NewRule("filegroup", "public_hdrs")
	public_hdrs.SetAttr("srcs", erlangApp.PublicHdrs.Values(strings.Compare))
	rules = append(rules, public_hdrs)

	priv := rule.NewRule("filegroup", "priv")
	priv.SetAttr("srcs", erlangApp.Priv.Values(strings.Compare))
	rules = append(rules, priv)

	licenses := rule.NewRule("filegroup", "licenses")
	licenses.SetAttr("srcs", erlangApp.LicenseFiles.Values(strings.Compare))
	rules = append(rules, licenses)

	hdrs := rule.NewRule("filegroup", "public_and_private_hdrs")
	hdrs.SetAttr("srcs", []string{
		":private_hdrs",
		":public_hdrs",
	})
	rules = append(rules, hdrs)

	all_srcs := rule.NewRule("filegroup", "all_srcs")
	all_srcs.SetAttr("srcs", []string{
		":srcs",
		":public_and_private_hdrs",
		// ":priv",
	})
	rules = append(rules, all_srcs)

	return rules
}

func (erlangApp *erlangApp) erlangAppRule(explicitFiles bool) *rule.Rule {
	r := rule.NewRule(erlangAppKind, "erlang_app")
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
	if !erlangApp.PublicHdrs.IsEmpty() {
		r.SetAttr("hdrs", []string{":public_hdrs"})
	}
	r.SetAttr("srcs", []string{":all_srcs"})

	if explicitFiles && !erlangApp.LicenseFiles.IsEmpty() {
		r.SetAttr("extra_license_files", erlangApp.LicenseFiles.Values(strings.Compare))
	}

	if !erlangApp.Deps.IsEmpty() {
		r.SetAttr("deps", erlangApp.Deps.Values(strings.Compare))
	}
	return r
}

func (erlangApp *erlangApp) testErlangAppRule(explicitFiles bool) *rule.Rule {
	r := rule.NewRule(testErlangAppKind, "test_erlang_app")
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
	if !erlangApp.PublicHdrs.IsEmpty() || !erlangApp.PrivateHdrs.IsEmpty() {
		r.SetAttr("hdrs", []string{":public_and_private_hdrs"})
	}
	r.SetAttr("srcs", []string{":all_srcs"})

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

func (erlangApp *erlangApp) testPathFor(from, include string) string {
	standardPath := erlangApp.pathFor(from, include)
	if standardPath != "" {
		return standardPath
	}
	directPath := filepath.Join(filepath.Dir(from), include)
	if erlangApp.TestHdrs.Contains(directPath) {
		return directPath
	}
	testPath := filepath.Join("test", include)
	if erlangApp.PrivateHdrs.Contains(testPath) {
		return testPath
	}
	return ""
}

func (erlangApp *erlangApp) testDirBeamFilesRules(args language.GenerateArgs, erlParser *erlParser) []*rule.Rule {
	erlangConfig := erlangConfigForRel(args.Config, args.Rel)

	var beamFilesRules []*rule.Rule
	outs := NewMutableSet[string]()
	for _, src := range erlangApp.TestSrcs.Values(strings.Compare) {
		actualPath := filepath.Join(erlangApp.RepoRoot, erlangApp.Rel, src)
		Log(args.Config, "        Parsing", src, "->", actualPath)
		erlAttrs, err := erlParser.deepParseErl(src, erlangApp)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}

		theseHdrs := NewMutableSet[string]()
		for _, include := range erlAttrs.Include {
			path := erlangApp.testPathFor(src, include)
			if path != "" {
				Log(args.Config, "            include", path)
				theseHdrs.Add(path)
			} else {
				Log(args.Config, "            ignoring include",
					include, "as it cannot be found")
			}
		}

		theseDeps := NewMutableSet[string]()
		for _, include := range erlAttrs.IncludeLib {
			parts := strings.Split(include, string(os.PathSeparator))
			if len(parts) > 0 {
				if !erlangConfig.IgnoredDeps[parts[0]] {
					Log(args.Config, "            include_lib", include, "->", parts[0])
					theseDeps.Add(parts[0])
				} else {
					Log(args.Config, "            ignoring include_lib", include)
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

func (erlangApp *erlangApp) xrefRule() *rule.Rule {
	r := rule.NewRule(xrefKind, "xref")
	r.SetAttr("target", ":erlang_app")
	return r
}

func (erlangApp *erlangApp) appPltRule() *rule.Rule {
	r := rule.NewRule(pltKind, "deps_plt")
	r.SetAttr("plt", "//:base_plt")
	r.SetAttr("for_target", ":erlang_app")
	return r
}

func (erlangApp *erlangApp) dialyzeRule() *rule.Rule {
	r := rule.NewRule(dialyzeKind, "dialyze")
	r.SetAttr("target", ":erlang_app")
	r.SetAttr("plt", ":deps_plt")
	return r
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
			if strings.HasSuffix(tm, "_tests") {
				if _, ok := modMap[strings.TrimSuffix(tm, "_tests")]; ok {
					modMap[strings.TrimSuffix(tm, "_tests")] = label
				} else {
					modMap[tm] = label
				}
			} else {
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
