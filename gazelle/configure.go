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
)

var (
	defaultIgnoredDeps = map[string]bool{
		"kernel":     true,
		"eunit":      true,
		"public_key": true,
	}
)

type ErlangConfig struct {
	Rel                         string
	Verbose                     bool
	AppsDir                     string
	BehaviourMappings           map[string]string
	ExcludeWhenRuleOfKindExists map[string]bool
	IgnoredDeps                 map[string]bool
	GenerateBeamFilesMacro      bool
}

type ErlangConfigs map[string]*ErlangConfig

func (erlang *Configurer) defaultErlangConfig(rel string) *ErlangConfig {
	return &ErlangConfig{
		Rel:                         rel,
		Verbose:                     erlang.verbose,
		AppsDir:                     erlang.appsDir,
		BehaviourMappings:           make(map[string]string),
		ExcludeWhenRuleOfKindExists: map[string]bool{"erlang_app": true},
		IgnoredDeps:                 defaultIgnoredDeps,
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
			AppsDir:                     parentConfig.AppsDir,
			BehaviourMappings:           CopyMap(parentConfig.BehaviourMappings),
			ExcludeWhenRuleOfKindExists: CopyMap(parentConfig.ExcludeWhenRuleOfKindExists),
			IgnoredDeps:                 CopyMap(parentConfig.IgnoredDeps),
			GenerateBeamFilesMacro:      parentConfig.GenerateBeamFilesMacro,
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
	fs.StringVar(&erlang.appsDir, "apps_dir", "apps", "directory containing embedded applications in an umbrella project")
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
			}
		}
		// Log(c, "    ", erlangConfig)
	}
}
