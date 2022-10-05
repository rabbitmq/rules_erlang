package erlang

import (
	"flag"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

type ErlangConfig struct {
	Verbose bool
}

type Configurer struct {
	verbose bool
}

func (erlang *Configurer) RegisterFlags(fs *flag.FlagSet, cmd string, c *config.Config) {
	fs.BoolVar(&erlang.verbose, "verbose", false, "when true, the erlang extension will log additional output")
}

func (erlang *Configurer) CheckFlags(fs *flag.FlagSet, c *config.Config) error {
	c.Exts[languageName] = ErlangConfig{Verbose: erlang.verbose}
	return nil
}

func (erlang *Configurer) KnownDirectives() []string {
	return []string{}
}

func (erlang *Configurer) Configure(c *config.Config, rel string, f *rule.File) {
	if f != nil {
		Log(c, "Configure:", rel, f.Path)
	} else {
		Log(c, "Configure:", rel, f)
	}
}
