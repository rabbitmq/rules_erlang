package erlang

import (
	"flag"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

const (
	behaviourSourceDirective             = "erlang_behaviour_source_lib"
	excludeWhenRuleOfKindExistsDirective = "erlang_exclude_when_rule_of_kind_exists"
	generateBeamFilesMacroDirective      = "erlang_generate_beam_files_macro"
	generateTestBeamUnconditionally      = "erlang_always_generate_test_beam_files"
	generateSkipRules                    = "erlang_skip_rules"
	localAppsDirsDirective               = "erlang_apps_dirs"
	erlangAppDepDirective                = "erlang_app_dep"
	erlangAppDepIgnoreDirective          = "erlang_app_dep_ignore"
	erlangAppExtraAppDirective           = "erlang_app_extra_app"
	erlangNoTestsDirective               = "erlang_no_tests"
	erlangErlcOptDirective               = "erlang_erlc_opt"
)

var (
	// https://www.erlang.org/doc/applications.html
	defaultIgnoredDeps = NewMutableSet(
		// Basic
		"compiler",
		"erts",
		"kernel",
		"sasl",
		"stdlib",
		// Database
		"mnesia",
		"odbc",
		// Operation & Maintenance
		"os_mon",
		"snmp",
		// Interface and Communication
		"asn1",
		"crypto",
		"diameter",
		"eldap",
		"erl_interface",
		"ftp",
		"inets",
		"jinterface",
		"megaco",
		"public_key",
		"ssh",
		"ssl",
		"tftp",
		"wx",
		"xmerl",
		// Tools
		"debugger",
		"dialyzer",
		"et",
		"observer",
		"parsetools",
		"reltool",
		"runtime_tools",
		"syntax_tools",
		"tools",
		// Test
		"common_test",
		"eunit",
		// Documentation
		"edoc",
		"erl_docgen",
	)
	defaultErlcOpts     = NewMutableSet("+debug_info")
	defaultTestErlcOpts = NewMutableSet("+debug_info", "-DTEST=1")
)

type ErlangConfig struct {
	Rel                             string
	Verbose                         bool
	NoTests                         bool
	BuildFilesDir                   string
	AppsDirs                        MutableSet[string]
	BehaviourMappings               map[string]string
	ExcludeWhenRuleOfKindExists     MutableSet[string]
	IgnoredDeps                     MutableSet[string]
	GenerateBeamFilesMacro          bool
	GenerateTestBeamUnconditionally bool
	GenerateSkipRules               MutableSet[string]
	Deps                            MutableSet[string]
	ExtraApps                       MutableSet[string]
	ErlcOpts                        MutableSet[string]
	TestErlcOpts                    MutableSet[string]
}

type ErlangConfigs map[string]*ErlangConfig

func (erlang *Configurer) defaultErlangConfig(rel string) *ErlangConfig {
	return &ErlangConfig{
		Rel:                             rel,
		Verbose:                         erlang.verbose,
		NoTests:                         erlang.noTests,
		BuildFilesDir:                   erlang.buildFilesDir,
		AppsDirs:                        NewMutableSet(erlang.appsDir),
		BehaviourMappings:               make(map[string]string),
		ExcludeWhenRuleOfKindExists:     NewMutableSet[string](),
		IgnoredDeps:                     defaultIgnoredDeps,
		GenerateBeamFilesMacro:          false,
		GenerateTestBeamUnconditionally: false,
		GenerateSkipRules:               NewMutableSet[string](),
		Deps:                            NewMutableSet[string](),
		ExtraApps:                       NewMutableSet[string](),
		ErlcOpts:                        Copy(defaultErlcOpts),
		TestErlcOpts:                    Copy(defaultTestErlcOpts),
	}
}

func erlangConfigForRel(c *config.Config, rel string) *ErlangConfig {
	configs := c.Exts[languageName].(ErlangConfigs)
	if _, ok := configs[rel]; !ok {
		dir, _ := filepath.Split(rel)
		parentRel := filepath.Clean(dir)
		if parentRel == "." {
			parentRel = ""
		}
		parentConfig := configs[parentRel]
		configs[rel] = &ErlangConfig{
			Rel:                             rel,
			Verbose:                         parentConfig.Verbose,
			NoTests:                         parentConfig.NoTests,
			BuildFilesDir:                   parentConfig.BuildFilesDir,
			AppsDirs:                        Copy(parentConfig.AppsDirs),
			BehaviourMappings:               CopyMap(parentConfig.BehaviourMappings),
			ExcludeWhenRuleOfKindExists:     Copy(parentConfig.ExcludeWhenRuleOfKindExists),
			IgnoredDeps:                     Copy(parentConfig.IgnoredDeps),
			GenerateBeamFilesMacro:          parentConfig.GenerateBeamFilesMacro,
			GenerateTestBeamUnconditionally: parentConfig.GenerateTestBeamUnconditionally,
			GenerateSkipRules:               Copy(parentConfig.GenerateSkipRules),
			Deps:                            NewMutableSet[string](),
			ExtraApps:                       NewMutableSet[string](),
			ErlcOpts:                        Copy(defaultErlcOpts),
			TestErlcOpts:                    Copy(defaultTestErlcOpts),
		}
	}
	return configs[rel]
}

type Configurer struct {
	verbose       bool
	noTests       bool
	appsDir       string
	buildFilesDir string
}

func (erlang *Configurer) RegisterFlags(fs *flag.FlagSet, cmd string, c *config.Config) {
	fs.BoolVar(&erlang.verbose, "verbose", false, "when true, the erlang extension will log additional output")
	fs.BoolVar(&erlang.noTests, "no_tests", false, "when true, generates no rules associated with testing")
	fs.StringVar(&erlang.appsDir, "default_apps_dir", "apps", "directory containing embedded applications in an umbrella project")
	if cmd == "update-repos" {
		fs.StringVar(&erlang.buildFilesDir, "build_files_dir", "", "directory to place BUILD.lib_name files when running update-repos")
	}
}

func (erlang *Configurer) CheckFlags(fs *flag.FlagSet, c *config.Config) error {
	configs := make(ErlangConfigs)
	configs[""] = erlang.defaultErlangConfig("")
	c.Exts[languageName] = configs
	return nil
}

func (erlang *Configurer) KnownDirectives() []string {
	return []string{
		behaviourSourceDirective,
		excludeWhenRuleOfKindExistsDirective,
		generateBeamFilesMacroDirective,
		generateSkipRules,
		generateTestBeamUnconditionally,
		localAppsDirsDirective,
		erlangAppDepDirective,
		erlangAppDepIgnoreDirective,
		erlangAppExtraAppDirective,
		erlangNoTestsDirective,
		erlangErlcOptDirective,
	}
}

func boolValue(d rule.Directive) bool {
	if d.Value == "" {
		return true
	}
	v, _ := strconv.ParseBool(d.Value)
	return v
}

func (erlang *Configurer) Configure(c *config.Config, rel string, f *rule.File) {
	erlangConfig := erlangConfigForRel(c, rel)

	if f == nil {
		Log(c, "Configure:", rel, f)
	} else {
		Log(c, "Configure:", rel, f.Path)

		for _, d := range f.Directives {
			switch d.Key {
			case behaviourSourceDirective:
				parts := strings.Split(d.Value, ":")
				behaviour := parts[0]
				dep := parts[1]
				erlangConfig.BehaviourMappings[behaviour] = dep
			case excludeWhenRuleOfKindExistsDirective:
				erlangConfig.ExcludeWhenRuleOfKindExists[d.Value] = true
			case generateBeamFilesMacroDirective:
				erlangConfig.GenerateBeamFilesMacro = boolValue(d)
			case generateTestBeamUnconditionally:
				erlangConfig.GenerateTestBeamUnconditionally = boolValue(d)
			case generateSkipRules:
				rules := strings.Split(d.Value, ",")
				erlangConfig.GenerateSkipRules.Add(rules...)
			case localAppsDirsDirective:
				dirs := Map(func(d string) string {
					return filepath.Join(rel, d)
				}, filepath.SplitList(d.Value))
				erlangConfig.AppsDirs.Add(dirs...)
			case erlangAppDepDirective:
				erlangConfig.Deps.Add(d.Value)
			case erlangAppDepIgnoreDirective:
				erlangConfig.IgnoredDeps.Add(d.Value)
			case erlangAppExtraAppDirective:
				erlangConfig.ExtraApps.Add(d.Value)
			case erlangNoTestsDirective:
				erlangConfig.NoTests = boolValue(d)
			case erlangErlcOptDirective:
				erlangConfig.ErlcOpts.Add(d.Value)
				erlangConfig.TestErlcOpts.Add(d.Value)
			}
		}
		// Log(c, "    ", erlangConfig)
	}
}
