package erlang

import (
	"flag"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

const (
	behaviourSourceDirective             = "erlang_behaviour_source_lib"
	excludeWhenRuleOfKindExistsDirective = "erlang_exclude_when_rule_of_kind_exists"
)

type ErlangConfig struct {
	Verbose                     bool
	BehaviourMappings           map[string]string
	ExcludeWhenRuleOfKindExists map[string]string
}

type Configurer struct {
	verbose bool
}

func (erlang *Configurer) RegisterFlags(fs *flag.FlagSet, cmd string, c *config.Config) {
	fs.BoolVar(&erlang.verbose, "verbose", false, "when true, the erlang extension will log additional output")
}

func (erlang *Configurer) CheckFlags(fs *flag.FlagSet, c *config.Config) error {
	c.Exts[languageName] = ErlangConfig{
		Verbose:                     erlang.verbose,
		BehaviourMappings:           make(map[string]string),
		ExcludeWhenRuleOfKindExists: map[string]string{"erlang_app": ""},
	}
	return nil
}

func (erlang *Configurer) KnownDirectives() []string {
	return []string{
		behaviourSourceDirective,
		excludeWhenRuleOfKindExistsDirective,
	}
}

func (erlang *Configurer) Configure(c *config.Config, rel string, f *rule.File) {
	erlangConfig := c.Exts[languageName].(ErlangConfig)

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
				erlangConfig.ExcludeWhenRuleOfKindExists[d.Value] = ""
			}
		}
	}
}
