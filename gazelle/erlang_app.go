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
			path := pathFor(erlangApp, include)
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

		erlang_bytecode := rule.NewRule("erlang_bytecode", ruleName(out))
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

		test_erlang_bytecode := rule.NewRule("erlang_bytecode", ruleName(test_out))
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
