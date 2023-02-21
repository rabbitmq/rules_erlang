package erlang

import (
	"errors"
	"log"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
	"github.com/rabbitmq/rules_erlang/gazelle/mutable_set"
)

type ErlangApp struct {
	RepoRoot     string
	Rel          string
	Name         string
	Description  string
	Version      string
	Ebin         mutable_set.MutableSet[string]
	Srcs         mutable_set.MutableSet[string]
	PrivateHdrs  mutable_set.MutableSet[string]
	PublicHdrs   mutable_set.MutableSet[string]
	AppSrc       mutable_set.MutableSet[string]
	TestSrcs     mutable_set.MutableSet[string]
	TestHdrs     mutable_set.MutableSet[string]
	Priv         mutable_set.MutableSet[string]
	LicenseFiles mutable_set.MutableSet[string]
	ErlcOpts     mutable_set.MutableSet[string]
	TestErlcOpts mutable_set.MutableSet[string]
	Deps         mutable_set.MutableSet[string]
	TestDeps     mutable_set.MutableSet[string]
	ExtraApps    mutable_set.MutableSet[string]
}

var ignoredIncludeLoggingPattern = regexp.MustCompile(`/lib/[^-]+-[^/]+/include/`)

func NewErlangApp(repoRoot, rel string) *ErlangApp {
	return &ErlangApp{
		RepoRoot:     repoRoot,
		Rel:          rel,
		Ebin:         mutable_set.New[string](),
		Srcs:         mutable_set.New[string](),
		PrivateHdrs:  mutable_set.New[string](),
		PublicHdrs:   mutable_set.New[string](),
		AppSrc:       mutable_set.New[string](),
		TestSrcs:     mutable_set.New[string](),
		TestHdrs:     mutable_set.New[string](),
		Priv:         mutable_set.New[string](),
		LicenseFiles: mutable_set.New[string](),
		ErlcOpts:     mutable_set.New[string](),
		TestErlcOpts: mutable_set.New[string](),
		Deps:         mutable_set.New[string](),
		TestDeps:     mutable_set.New[string](),
		ExtraApps:    mutable_set.New[string](),
	}
}

func (erlangApp *ErlangApp) AddFile(f string) {
	if strings.HasPrefix(f, "ebin/") {
		if strings.HasSuffix(f, ".app") {
			erlangApp.Ebin.Add(f)
		}
		// TODO: handle .appup files
	} else if strings.HasPrefix(f, "src/") {
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
	r := "ebin/" + filepath.Base(src)
	return strings.TrimSuffix(r, ".erl") + ".beam"
}

func testBeamFile(src string) string {
	r := "test/" + filepath.Base(src)
	return strings.TrimSuffix(r, ".erl") + ".beam"
}

func ruleName(f string) string {
	r := strings.ReplaceAll(f, string(filepath.Separator), "_")
	return strings.ReplaceAll(r, ".", "_")
}

func (erlangApp *ErlangApp) pathFor(from, include string) string {
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

func erlcOptsWithSelect(opts mutable_set.MutableSet[string]) rule.SelectStringListValue {
	debugOpts := mutable_set.Copy(opts)
	defaultOpts := mutable_set.Copy(opts)
	defaultOpts.Add("+deterministic")
	return rule.SelectStringListValue{
		"@rules_erlang//:debug_build": debugOpts.Values(strings.Compare),
		"//conditions:default":        defaultOpts.Values(strings.Compare),
	}
}

func (erlangApp *ErlangApp) ErlcOptsRule(args language.GenerateArgs) *rule.Rule {
	erlangConfig := erlangConfigForRel(args.Config, args.Rel)
	erlc_opts := rule.NewRule(erlcOptsKind, erlcOptsRuleName)
	erlc_opts.SetAttr("values",
		erlcOptsWithSelect(mutable_set.Union(erlangConfig.ErlcOpts, erlangApp.ErlcOpts)))
	erlc_opts.SetAttr("visibility", []string{":__subpackages__"})
	return erlc_opts
}

func (erlangApp *ErlangApp) testErlcOptsRule(args language.GenerateArgs) *rule.Rule {
	erlangConfig := erlangConfigForRel(args.Config, args.Rel)
	test_erlc_opts := rule.NewRule(erlcOptsKind, testErlcOptsRuleName)
	test_erlc_opts.SetAttr("values",
		erlcOptsWithSelect(mutable_set.Union(erlangConfig.TestErlcOpts, erlangApp.TestErlcOpts)))
	test_erlc_opts.SetAttr("visibility", []string{":__subpackages__"})
	return test_erlc_opts
}

func (erlangApp *ErlangApp) basePltRule() *rule.Rule {
	plt := rule.NewRule(pltKind, "base_plt")
	plt.SetAttr("visibility", []string{":__subpackages__"})
	return plt
}

func macros(erlcOpts mutable_set.MutableSet[string]) ErlParserMacros {
	r := make(ErlParserMacros)
	erlcOpts.ForEach(func(opt string) {
		if strings.HasPrefix(opt, "-D") {
			parts := strings.Split(strings.TrimPrefix(opt, "-D"), "=")
			if len(parts) == 1 {
				r[parts[0]] = nil
			} else {
				r[parts[0]] = &parts[1]
			}
		}
	})
	return r
}

func (erlangApp *ErlangApp) parseSrcs(args language.GenerateArgs, erlParser ErlParser, erlcOpts mutable_set.MutableSet[string], srcs mutable_set.MutableSet[string]) map[string]*ErlAttrs {
	r := make(map[string]*ErlAttrs)
	for _, src := range srcs.Values(strings.Compare) {
		actualPath := filepath.Join(erlangApp.RepoRoot, erlangApp.Rel, src)
		erlAttrs, err := erlParser.DeepParseErl(src, erlangApp, macros(erlcOpts))
		if errors.Is(err, os.ErrNotExist) {
			Log(args.Config, "        Skipped (not found)", src, "->", actualPath)
			r[src] = &ErlAttrs{}
		} else {
			Log(args.Config, "        Parsed", src, "->", actualPath)
			if err != nil {
				log.Fatalf("ERROR: %v\n", err)
			}
			r[src] = erlAttrs
		}
	}
	return r
}

type depsInfo struct {
	Hdrs        mutable_set.MutableSet[string]
	Beam        mutable_set.MutableSet[string]
	Deps        mutable_set.MutableSet[string]
	RuntimeDeps mutable_set.MutableSet[string]
}

func (erlangApp *ErlangApp) dependencies(config *config.Config, erlangConfig *ErlangConfig, moduleindex Moduleindex, erlAttrsBySrc map[string]*ErlAttrs, srcs ...string) depsInfo {
	info := depsInfo{
		Hdrs:        mutable_set.New[string](),
		Beam:        mutable_set.New[string](),
		Deps:        mutable_set.New[string](),
		RuntimeDeps: mutable_set.New[string](),
	}

	for _, src := range srcs {
		Log(config, "        Analyzing", src)

		erlAttrs := erlAttrsBySrc[src]

		for _, include := range erlAttrs.Include {
			path := erlangApp.pathFor(src, include)
			if path != "" {
				Log(config, "            include", path)
				info.Hdrs.Add(path)
			} else if !ignoredIncludeLoggingPattern.MatchString(include) {
				Log(config, "            ignoring include",
					include, "as it cannot be found")
			}
		}

		for _, include := range erlAttrs.IncludeLib {
			path := erlangApp.pathFor(src, include)
			if path != "" {
				Log(config, "            include_lib", path)
				info.Hdrs.Add(path)
			} else if parts := strings.Split(include, string(os.PathSeparator)); len(parts) > 0 {
				if parts[0] == erlangApp.Name {
					path := erlangApp.pathFor(src, strings.Join(parts[1:], string(os.PathSeparator)))
					if path != "" {
						Log(config, "            include_lib (self)", path)
						info.Hdrs.Add(path)
					} else {
						Log(config, "            ignoring include_lib (self)",
							include, "as it cannot be found")
					}
				} else if !erlangConfig.IgnoredDeps.Contains(parts[0]) {
					Log(config, "            include_lib", include, "->", parts[0])
					info.Deps.Add(parts[0])
				} else {
					Log(config, "            ignoring include_lib", include)
				}
			}
		}

		for _, module := range erlAttrs.modules() {
			found := false
			for _, other_src := range erlangApp.Srcs.Values(strings.Compare) {
				if moduleName(other_src) == module {
					Log(config, "            module", module, "->", beamFile(src))
					info.Beam.Add(beamFile(other_src))
					found = true
					break
				}
			}
			if found {
				continue
			}
			if dep, found := erlangConfig.ModuleMappings[module]; found {
				Log(config, "            module", module, "->", dep)
				info.Deps.Add(dep)
				continue
			}
			if app := FindModule(moduleindex, module); app != "" && app != erlangApp.Name {
				Log(config, "            module", module, "->", app)
				info.Deps.Add(app)
				continue
			}
		}

		for module := range erlAttrs.Call {
			app := erlangConfig.ModuleMappings[module]
			if app == "" {
				app = FindModule(moduleindex, module)
			}
			if app == "" {
				Log(config, "            ignoring call", module)
			} else if app == erlangApp.Name {
				Log(config, "            self call", module, "->", app)
			} else if erlangConfig.IgnoredDeps.Contains(app) {
				Log(config, "            ignoring call", module, "->", app, "(explicit ignore)")
			} else {
				Log(config, "            call", module, "->", app)
				info.RuntimeDeps.Add(app)
			}
		}
	}

	return info
}

func (erlangApp *ErlangApp) BeamFilesRules(args language.GenerateArgs, erlParser ErlParser) (beamFilesRules []*rule.Rule) {
	erlangConfig := erlangConfigForRel(args.Config, args.Rel)

	ownModules := mutable_set.Map(erlangApp.Srcs, moduleName)

	moduleindex, err := ReadModuleindex(filepath.Join(args.Config.RepoRoot, "moduleindex.yaml"))
	if err != nil {
		moduleindex = Moduleindex{erlangApp.Name: ownModules.Values(strings.Compare)}
	}

	erlcOpts := mutable_set.Union(erlangConfig.ErlcOpts, erlangApp.ErlcOpts)
	erlAttrsBySrc := erlangApp.parseSrcs(args, erlParser, erlcOpts, erlangApp.Srcs)

	if erlangConfig.GenerateFewerBytecodeRules {
		transforms := mutable_set.New[string]()
		behaviours := mutable_set.New[string]()

		hdrs := erlangApp.Hdrs()

		srcByModuleName := mutable_set.Index(erlangApp.Srcs, moduleName)

		for _, src := range erlangApp.Srcs.Values(strings.Compare) {
			erlAttrs := erlAttrsBySrc[src]

			for _, module := range erlAttrs.ParseTransform {
				if other_src, ok := srcByModuleName[module]; ok {
					transforms.Add(other_src)
				}
			}
			for _, module := range erlAttrs.Behaviour {
				if other_src, ok := srcByModuleName[module]; ok {
					behaviours.Add(other_src)
				}
			}
		}

		others := mutable_set.Copy(erlangApp.Srcs)
		others.Subtract(transforms)
		others.Subtract(behaviours)

		var xformsRule, behavioursRule, othersRule *rule.Rule
		var beamFilesGroupRules []string
		if !transforms.IsEmpty() {
			xformsRule = rule.NewRule(erlangBytecodeKind, "parse_transforms")
			xformsRule.SetAttr("app_name", erlangApp.Name)
			xformsRule.SetAttr("erlc_opts", "//:"+erlcOptsRuleName)
			xformsRule.SetAttr("srcs", transforms.Values(strings.Compare))
			if len(hdrs) > 0 {
				xformsRule.SetAttr("hdrs", hdrs)
			}
			xformsRule.SetAttr("outs", mutable_set.Map(transforms, beamFile).Values(strings.Compare))
			xformsDeps := erlangApp.dependencies(args.Config, erlangConfig, moduleindex, erlAttrsBySrc, transforms.ValuesUnordered()...)
			if !xformsDeps.Deps.IsEmpty() {
				xformsRule.SetAttr("deps", xformsDeps.Deps.Values(strings.Compare))
			}
			erlangApp.Deps.Union(xformsDeps.Deps)
			erlangApp.Deps.Union(xformsDeps.RuntimeDeps)
			beamFilesRules = append(beamFilesRules, xformsRule)
			beamFilesGroupRules = append(beamFilesGroupRules, ":"+xformsRule.Name())
		}

		if !behaviours.IsEmpty() {
			behavioursRule = rule.NewRule(erlangBytecodeKind, "behaviours")
			behavioursRule.SetAttr("app_name", erlangApp.Name)
			behavioursRule.SetAttr("erlc_opts", "//:"+erlcOptsRuleName)
			behavioursRule.SetAttr("srcs", behaviours.Values(strings.Compare))
			if len(hdrs) > 0 {
				behavioursRule.SetAttr("hdrs", hdrs)
			}
			behavioursRule.SetAttr("outs", mutable_set.Map(behaviours, beamFile).Values(strings.Compare))
			if len(beamFilesGroupRules) > 0 {
				behavioursRule.SetAttr("beam", beamFilesGroupRules)
			}
			behavioursDeps := erlangApp.dependencies(args.Config, erlangConfig, moduleindex, erlAttrsBySrc, behaviours.ValuesUnordered()...)
			if !behavioursDeps.Deps.IsEmpty() {
				behavioursRule.SetAttr("deps", behavioursDeps.Deps.Values(strings.Compare))
			}
			erlangApp.Deps.Union(behavioursDeps.Deps)
			erlangApp.Deps.Union(behavioursDeps.RuntimeDeps)
			beamFilesRules = append(beamFilesRules, behavioursRule)
			beamFilesGroupRules = append(beamFilesGroupRules, ":"+behavioursRule.Name())
		}

		if !others.IsEmpty() {
			othersRule = rule.NewRule(erlangBytecodeKind, "other_beam")
			othersRule.SetAttr("app_name", erlangApp.Name)
			othersRule.SetAttr("erlc_opts", "//:"+erlcOptsRuleName)
			othersRule.SetAttr("srcs", others.Values(strings.Compare))
			if len(hdrs) > 0 {
				othersRule.SetAttr("hdrs", hdrs)
			}
			othersRule.SetAttr("outs", mutable_set.Map(others, beamFile).Values(strings.Compare))

			othersRule.SetAttr("beam", beamFilesGroupRules)
			othersDeps := erlangApp.dependencies(args.Config, erlangConfig, moduleindex, erlAttrsBySrc, others.ValuesUnordered()...)
			if !othersDeps.Deps.IsEmpty() {
				othersRule.SetAttr("deps", othersDeps.Deps.Values(strings.Compare))
			}
			erlangApp.Deps.Union(othersDeps.Deps)
			erlangApp.Deps.Union(othersDeps.RuntimeDeps)
			beamFilesRules = append(beamFilesRules, othersRule)
			beamFilesGroupRules = append(beamFilesGroupRules, ":"+othersRule.Name())
		}

		beam_files := rule.NewRule("filegroup", "beam_files")
		beam_files.SetAttr("srcs", beamFilesGroupRules)
		beamFilesRules = append(beamFilesRules, beam_files)
	} else {
		outs := mutable_set.New[string]()
		for _, src := range erlangApp.Srcs.Values(strings.Compare) {
			deps := erlangApp.dependencies(args.Config, erlangConfig, moduleindex, erlAttrsBySrc, src)

			erlangApp.Deps.Union(deps.Deps)
			erlangApp.Deps.Union(deps.RuntimeDeps)

			out := beamFile(src)
			outs.Add(out)

			erlang_bytecode := rule.NewRule(erlangBytecodeKind, ruleName(out))
			erlang_bytecode.SetAttr("app_name", erlangApp.Name)
			erlang_bytecode.SetAttr("srcs", []interface{}{src})
			if !deps.Hdrs.IsEmpty() {
				erlang_bytecode.SetAttr("hdrs", deps.Hdrs.Values(strings.Compare))
			}
			erlang_bytecode.SetAttr("erlc_opts", "//:"+erlcOptsRuleName)
			erlang_bytecode.SetAttr("outs", []string{out})
			if !deps.Beam.IsEmpty() {
				erlang_bytecode.SetAttr("beam", deps.Beam.Values(strings.Compare))
			}
			if !deps.Deps.IsEmpty() {
				erlang_bytecode.SetAttr("deps", deps.Deps.Values(strings.Compare))
			}

			beamFilesRules = append(beamFilesRules, erlang_bytecode)
		}

		beam_files := rule.NewRule("filegroup", "beam_files")
		beam_files.SetAttr("srcs", outs.Values(strings.Compare))
		beamFilesRules = append(beamFilesRules, beam_files)
	}

	return
}

func (erlangApp *ErlangApp) testBeamFilesRules(args language.GenerateArgs, erlParser ErlParser) (testBeamFilesRules []*rule.Rule) {
	erlangConfig := erlangConfigForRel(args.Config, args.Rel)

	ownModules := mutable_set.Map(erlangApp.Srcs, moduleName)

	moduleindex, err := ReadModuleindex(filepath.Join(args.Config.RepoRoot, "moduleindex.yaml"))
	if err != nil {
		moduleindex = map[string][]string{erlangApp.Name: ownModules.Values(strings.Compare)}
	}

	testErlcOpts := mutable_set.Union(erlangConfig.TestErlcOpts, erlangApp.TestErlcOpts)
	erlAttrsBySrc := erlangApp.parseSrcs(args, erlParser, testErlcOpts, erlangApp.Srcs)

	if erlangConfig.GenerateFewerBytecodeRules {
		transforms := mutable_set.New[string]()
		behaviours := mutable_set.New[string]()

		srcByModuleName := mutable_set.Index(erlangApp.Srcs, moduleName)

		hdrs := erlangApp.Hdrs()

		for _, src := range erlangApp.Srcs.Values(strings.Compare) {
			erlAttrs := erlAttrsBySrc[src]

			for _, module := range erlAttrs.ParseTransform {
				if other_src, ok := srcByModuleName[module]; ok {
					transforms.Add(other_src)
				}
			}
			for _, module := range erlAttrs.Behaviour {
				if other_src, ok := srcByModuleName[module]; ok {
					behaviours.Add(other_src)
				}
			}
		}

		others := mutable_set.Copy(erlangApp.Srcs)
		others.Subtract(transforms)
		others.Subtract(behaviours)

		var xformsRule, behavioursRule, othersRule *rule.Rule
		var beamFilesGroupRules []string
		if !transforms.IsEmpty() {
			xformsRule = rule.NewRule(erlangBytecodeKind, "test_parse_transforms")
			xformsRule.SetAttr("testonly", true)
			xformsRule.SetAttr("app_name", erlangApp.Name)
			xformsRule.SetAttr("erlc_opts", "//:"+testErlcOptsRuleName)
			xformsRule.SetAttr("srcs", transforms.Values(strings.Compare))
			if len(hdrs) > 0 {
				xformsRule.SetAttr("hdrs", hdrs)
			}
			xformsRule.SetAttr("outs", mutable_set.Map(transforms, testBeamFile).Values(strings.Compare))
			xformsDeps := erlangApp.dependencies(args.Config, erlangConfig, moduleindex, erlAttrsBySrc, transforms.ValuesUnordered()...)
			if !xformsDeps.Deps.IsEmpty() {
				xformsRule.SetAttr("deps", xformsDeps.Deps.Values(strings.Compare))
			}
			erlangApp.TestDeps.Union(xformsDeps.Deps)
			erlangApp.TestDeps.Union(xformsDeps.RuntimeDeps)
			testBeamFilesRules = append(testBeamFilesRules, xformsRule)
			beamFilesGroupRules = append(beamFilesGroupRules, ":"+xformsRule.Name())
		}

		if !behaviours.IsEmpty() {
			behavioursRule = rule.NewRule(erlangBytecodeKind, "test_behaviours")
			behavioursRule.SetAttr("testonly", true)
			behavioursRule.SetAttr("app_name", erlangApp.Name)
			behavioursRule.SetAttr("erlc_opts", "//:"+testErlcOptsRuleName)
			behavioursRule.SetAttr("srcs", behaviours.Values(strings.Compare))
			if len(hdrs) > 0 {
				behavioursRule.SetAttr("hdrs", hdrs)
			}
			behavioursRule.SetAttr("outs", mutable_set.Map(behaviours, testBeamFile).Values(strings.Compare))
			if len(beamFilesGroupRules) > 0 {
				behavioursRule.SetAttr("beam", beamFilesGroupRules)
			}
			behavioursDeps := erlangApp.dependencies(args.Config, erlangConfig, moduleindex, erlAttrsBySrc, behaviours.ValuesUnordered()...)
			if !behavioursDeps.Deps.IsEmpty() {
				behavioursRule.SetAttr("deps", behavioursDeps.Deps.Values(strings.Compare))
			}
			erlangApp.TestDeps.Union(behavioursDeps.Deps)
			erlangApp.TestDeps.Union(behavioursDeps.RuntimeDeps)
			testBeamFilesRules = append(testBeamFilesRules, behavioursRule)
			beamFilesGroupRules = append(beamFilesGroupRules, ":"+behavioursRule.Name())
		}

		if !others.IsEmpty() {
			othersRule = rule.NewRule(erlangBytecodeKind, "test_other_beam")
			othersRule.SetAttr("testonly", true)
			othersRule.SetAttr("app_name", erlangApp.Name)
			othersRule.SetAttr("erlc_opts", "//:"+testErlcOptsRuleName)
			othersRule.SetAttr("srcs", others.Values(strings.Compare))
			if len(hdrs) > 0 {
				othersRule.SetAttr("hdrs", hdrs)
			}
			othersRule.SetAttr("outs", mutable_set.Map(others, testBeamFile).Values(strings.Compare))
			if len(beamFilesGroupRules) > 0 {
				othersRule.SetAttr("beam", beamFilesGroupRules)
			}
			othersDeps := erlangApp.dependencies(args.Config, erlangConfig, moduleindex, erlAttrsBySrc, others.ValuesUnordered()...)
			if !othersDeps.Deps.IsEmpty() {
				othersRule.SetAttr("deps", othersDeps.Deps.Values(strings.Compare))
			}
			erlangApp.TestDeps.Union(othersDeps.Deps)
			erlangApp.TestDeps.Union(othersDeps.RuntimeDeps)
			testBeamFilesRules = append(testBeamFilesRules, othersRule)
			beamFilesGroupRules = append(beamFilesGroupRules, ":"+othersRule.Name())
		}

		test_beam_files := rule.NewRule("filegroup", "test_beam_files")
		test_beam_files.SetAttr("srcs", beamFilesGroupRules)
		test_beam_files.SetAttr("testonly", true)
		testBeamFilesRules = append(testBeamFilesRules, test_beam_files)
	} else {
		testOuts := mutable_set.New[string]()
		for _, src := range erlangApp.Srcs.Values(strings.Compare) {
			deps := erlangApp.dependencies(args.Config, erlangConfig, moduleindex, erlAttrsBySrc, src)

			erlangApp.Deps.Union(deps.Deps)
			erlangApp.Deps.Union(deps.RuntimeDeps)

			test_out := testBeamFile(src)
			testOuts.Add(test_out)

			test_erlang_bytecode := rule.NewRule(erlangBytecodeKind, ruleName(test_out))
			test_erlang_bytecode.SetAttr("app_name", erlangApp.Name)
			test_erlang_bytecode.SetAttr("srcs", []interface{}{src})
			if !deps.Hdrs.IsEmpty() {
				test_erlang_bytecode.SetAttr("hdrs", deps.Hdrs.Values(strings.Compare))
			}
			test_erlang_bytecode.SetAttr("erlc_opts", "//:"+testErlcOptsRuleName)
			test_erlang_bytecode.SetAttr("outs", []string{test_out})
			if !deps.Beam.IsEmpty() {
				test_erlang_bytecode.SetAttr("beam", deps.Beam.Values(strings.Compare))
			}
			if !deps.Deps.IsEmpty() {
				test_erlang_bytecode.SetAttr("deps", deps.Deps.Values(strings.Compare))
			}
			test_erlang_bytecode.SetAttr("testonly", true)

			testBeamFilesRules = append(testBeamFilesRules, test_erlang_bytecode)
		}

		test_beam_files := rule.NewRule("filegroup", "test_beam_files")
		test_beam_files.SetAttr("srcs", testOuts.Values(strings.Compare))
		test_beam_files.SetAttr("testonly", true)
		testBeamFilesRules = append(testBeamFilesRules, test_beam_files)
	}
	return
}

func (erlangApp *ErlangApp) allSrcsRules() []*rule.Rule {
	var rules []*rule.Rule

	srcs := rule.NewRule("filegroup", "srcs")
	srcs.SetAttr("srcs", mutable_set.Union(erlangApp.Srcs, erlangApp.AppSrc).Values(strings.Compare))
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

func (erlangApp *ErlangApp) ErlangAppRule(args language.GenerateArgs, explicitFiles bool) *rule.Rule {
	erlangConfig := erlangConfigForRel(args.Config, args.Rel)

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

	deps := erlangApp.Deps.Clone()
	deps.Subtract(erlangConfig.ExcludedDeps)
	if !deps.IsEmpty() {
		r.SetAttr("deps", deps.Values(strings.Compare))
	}
	return r
}

func (erlangApp *ErlangApp) testErlangAppRule(args language.GenerateArgs, explicitFiles bool) *rule.Rule {
	erlangConfig := erlangConfigForRel(args.Config, args.Rel)

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

	deps := mutable_set.Union(erlangApp.Deps, erlangApp.TestDeps)
	deps.Subtract(erlangConfig.ExcludedDeps)
	if !deps.IsEmpty() {
		r.SetAttr("deps", deps.Values(strings.Compare))
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

func (erlangApp *ErlangApp) testPathFor(from, include string) string {
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

func (erlangApp *ErlangApp) TestDirBeamFilesRules(args language.GenerateArgs, erlParser ErlParser) []*rule.Rule {
	erlangConfig := erlangConfigForRel(args.Config, args.Rel)

	ownModules := mutable_set.New[string]()
	for _, src := range erlangApp.Srcs.Values(strings.Compare) {
		ownModules.Add(moduleName(src))
	}

	moduleindex, err := ReadModuleindex(filepath.Join(args.Config.RepoRoot, "moduleindex.yaml"))
	if err != nil {
		moduleindex = map[string][]string{erlangApp.Name: ownModules.Values(strings.Compare)}
	}

	erlAttrsBySrc := erlangApp.parseSrcs(args, erlParser, erlangApp.TestErlcOpts, erlangApp.TestSrcs)

	var beamFilesRules []*rule.Rule
	outs := mutable_set.New[string]()
	for _, src := range erlangApp.TestSrcs.Values(strings.Compare) {
		erlAttrs := erlAttrsBySrc[src]

		Log(args.Config, "        Analyzing", src)

		theseHdrs := mutable_set.New[string]()
		for _, include := range erlAttrs.Include {
			path := erlangApp.testPathFor(src, include)
			if path != "" {
				Log(args.Config, "            include", path)
				theseHdrs.Add(path)
			} else if !ignoredIncludeLoggingPattern.MatchString(include) {
				Log(args.Config, "            ignoring include",
					include, "as it cannot be found")
			}
		}

		theseDeps := mutable_set.New[string]()
		for _, include := range erlAttrs.IncludeLib {
			path := erlangApp.pathFor(src, include)
			if path != "" {
				Log(args.Config, "            include_lib", path)
				theseHdrs.Add(path)
			} else if parts := strings.Split(include, string(os.PathSeparator)); len(parts) > 0 {
				if !erlangConfig.IgnoredDeps.Contains(parts[0]) {
					Log(args.Config, "            include_lib", include, "->", parts[0])
					theseDeps.Add(parts[0])
				} else {
					Log(args.Config, "            ignoring include_lib", include)
				}
			}
		}

		theseBeam := mutable_set.New[string]()
		for _, module := range erlAttrs.modules() {
			found := false
			for _, other_src := range erlangApp.Srcs.Values(strings.Compare) {
				if moduleName(other_src) == module {
					Log(args.Config, "            module", module, "->", beamFile(src))
					theseBeam.Add(beamFile(other_src))
					found = true
					break
				}
			}
			if found {
				continue
			}
			if dep, found := erlangConfig.ModuleMappings[module]; found {
				Log(args.Config, "            module", module, "->", dep)
				theseDeps.Add(dep)
				continue
			}
			if app := FindModule(moduleindex, module); app != "" && app != erlangApp.Name {
				Log(args.Config, "            module", module, "->", app)
				theseDeps.Add(app)
				continue
			}
		}

		theseRuntimeBeam := mutable_set.New[string]()
		theseRuntimeDeps := mutable_set.New[string]()
		for module := range erlAttrs.Call {
			found := false
			for _, other_src := range erlangApp.TestSrcs.Values(strings.Compare) {
				if moduleName(other_src) == module {
					Log(args.Config, "            module", module, "->", beamFile(src))
					theseRuntimeBeam.Add(strings.TrimSuffix(other_src, ".erl") + ".beam")
					found = true
					break
				}
			}
			if found {
				continue
			}
			app := erlangConfig.ModuleMappings[module]
			if app == "" {
				app = FindModule(moduleindex, module)
			}
			if app == "" {
				Log(args.Config, "            ignoring call", module)
			} else if app == erlangApp.Name {
				Log(args.Config, "            self call", module, "->", app)
			} else if erlangConfig.IgnoredDeps.Contains(app) {
				Log(args.Config, "            ignoring call", module, "->", app, "(explicit ignore)")
			} else {
				Log(args.Config, "            call", module, "->", app)
				theseRuntimeDeps.Add(app)
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
		erlang_bytecode.SetPrivateAttr("runtime_beam", theseRuntimeBeam.Values(strings.Compare))
		erlang_bytecode.SetPrivateAttr("runtime_deps", theseRuntimeDeps.Values(strings.Compare))
		erlang_bytecode.SetAttr("testonly", true)

		beamFilesRules = append(beamFilesRules, erlang_bytecode)
	}

	return beamFilesRules
}

func (erlangApp *ErlangApp) xrefRule() *rule.Rule {
	r := rule.NewRule(xrefKind, "xref")
	r.SetAttr("target", ":erlang_app")
	return r
}

func (erlangApp *ErlangApp) appPltRule() *rule.Rule {
	r := rule.NewRule(pltKind, "deps_plt")
	r.SetAttr("plt", "//:base_plt")
	r.SetAttr("for_target", ":erlang_app")
	return r
}

func (erlangApp *ErlangApp) dialyzeRule() *rule.Rule {
	r := rule.NewRule(dialyzeKind, "dialyze")
	r.SetAttr("target", ":erlang_app")
	r.SetAttr("plt", ":deps_plt")
	return r
}

func (erlangApp *ErlangApp) EunitRule() *rule.Rule {
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

	eunit_mods := mutable_set.New[string]()
	compiled_suites := mutable_set.New[string]()
	for mod, beam := range modMap {
		eunit_mods.Add(mod)
		if beam != "" {
			compiled_suites.Add(beam)
		}
	}

	eunit := rule.NewRule(eunitKind, "eunit")
	if !compiled_suites.IsEmpty() {
		eunit.SetAttr("compiled_suites", compiled_suites.Values(strings.Compare))
	}
	eunit.SetAttr("target", ":test_erlang_app")

	return eunit
}

func (erlangApp *ErlangApp) CtSuiteRules(testDirBeamFilesRules []*rule.Rule) []*rule.Rule {
	rulesByName := make(map[string]*rule.Rule, len(testDirBeamFilesRules))
	for _, r := range testDirBeamFilesRules {
		name := strings.TrimSuffix(r.Name(), "_beam_files")
		rulesByName[name] = r
	}

	var rules []*rule.Rule
	for _, testSrc := range erlangApp.TestSrcs.Values(strings.Compare) {
		modName := moduleName(testSrc)
		if strings.HasSuffix(modName, "_SUITE") {
			beamFilesRule := rulesByName[modName]
			r := rule.NewRule(ctTestKind, modName)
			r.SetAttr("compiled_suites",
				append([]string{":" + beamFilesRule.Name()},
					runtimeBeam(beamFilesRule)...))
			r.SetAttr("data", rule.GlobValue{
				Patterns: []string{"test/" + modName + "_data/**/*"},
			})
			deps := []string{":test_erlang_app"}
			deps = append(deps, runtimeDeps(beamFilesRule)...)
			if len(deps) > 0 {
				r.SetAttr("deps", deps)
			}

			rules = append(rules, r)
		}
	}

	return rules
}

func (erlangApp *ErlangApp) hasTestSuites() bool {
	return !erlangApp.TestSrcs.IsEmpty()
}

func (erlangApp *ErlangApp) Hdrs() []string {
	return mutable_set.Union(
		erlangApp.PrivateHdrs,
		erlangApp.PublicHdrs,
	).Values(strings.Compare)
}

func (erlangApp *ErlangApp) modules() []string {
	modules := make([]string, len(erlangApp.Srcs))
	for i, src := range erlangApp.Srcs.Values(strings.Compare) {
		modules[i] = strings.TrimSuffix(filepath.Base(src), ".erl")
	}
	return modules
}

func runtimeBeam(r *rule.Rule) []string {
	return r.PrivateAttr("runtime_beam").([]string)
}

func runtimeDeps(r *rule.Rule) []string {
	return r.PrivateAttr("runtime_deps").([]string)
}
