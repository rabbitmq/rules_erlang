package erlang

import (
	"flag"
	"path/filepath"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

const (
	behaviourSourceDirective             = "erlang_behaviour_source_lib"
	excludeWhenRuleOfKindExistsDirective = "erlang_exclude_when_rule_of_kind_exists"
	generateBeamFilesMacroDirective      = "erlang_generate_beam_files_macro"
	generateSkipRules                    = "erlang_skip_rules"
	localAppsDirsDirective               = "erlang_apps_dirs"
)

var (
	defaultIgnoredDeps = NewMutableSet(
		"kernel",
		"eunit",
		"public_key",
		"stdlib",
	)
)

type ErlangConfig struct {
	Rel                         string
	Verbose                     bool
	AppsDirs                    MutableSet[string]
	BehaviourMappings           map[string]string
	ExcludeWhenRuleOfKindExists MutableSet[string]
	IgnoredDeps                 MutableSet[string]
	GenerateBeamFilesMacro      bool
	GenerateSkipRules           MutableSet[string]
}

type ErlangConfigs map[string]*ErlangConfig

func (erlang *Configurer) defaultErlangConfig(rel string) *ErlangConfig {
	return &ErlangConfig{
		Rel:                         rel,
		Verbose:                     erlang.verbose,
		AppsDirs:                    NewMutableSet(erlang.appsDir),
		BehaviourMappings:           make(map[string]string),
		ExcludeWhenRuleOfKindExists: NewMutableSet[string](),
		IgnoredDeps:                 defaultIgnoredDeps,
		GenerateBeamFilesMacro:      false,
		GenerateSkipRules:           NewMutableSet[string](),
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
			Rel:                         rel,
			Verbose:                     parentConfig.Verbose,
			AppsDirs:                    Copy(parentConfig.AppsDirs),
			BehaviourMappings:           CopyMap(parentConfig.BehaviourMappings),
			ExcludeWhenRuleOfKindExists: Copy(parentConfig.ExcludeWhenRuleOfKindExists),
			IgnoredDeps:                 Copy(parentConfig.IgnoredDeps),
			GenerateBeamFilesMacro:      parentConfig.GenerateBeamFilesMacro,
			GenerateSkipRules:           Copy(parentConfig.GenerateSkipRules),
		}
	}
	return configs[rel]
}

type Configurer struct {
	verbose bool
	appsDir string
}

func (erlang *Configurer) RegisterFlags(fs *flag.FlagSet, cmd string, c *config.Config) {
	fs.BoolVar(&erlang.verbose, "verbose", false, "when true, the erlang extension will log additional output")
	fs.StringVar(&erlang.appsDir, "default_apps_dir", "apps", "directory containing embedded applications in an umbrella project")
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
		localAppsDirsDirective,
	}
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
				enabled := (d.Value == "" || strings.ToLower(d.Value) == "true")
				erlangConfig.GenerateBeamFilesMacro = enabled
			case generateSkipRules:
				rules := strings.Split(d.Value, ",")
				erlangConfig.GenerateSkipRules.Add(rules...)
			case localAppsDirsDirective:
				dirs := filepath.SplitList(d.Value)
				erlangConfig.AppsDirs.Add(dirs...)
			}
		}
		// Log(c, "    ", erlangConfig)
	}
}
