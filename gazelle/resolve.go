package erlang

import (
	"fmt"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/repo"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

const languageName = "erlang"

type Resolver struct{}

func (erlang *Resolver) Imports(c *config.Config, r *rule.Rule, f *rule.File) []resolve.ImportSpec {
	Log(c, "Imports:", f.Path)
	return nil
}

func (erlang *Resolver) Embeds(r *rule.Rule, from label.Label) []label.Label {
	// TODO(f0rmiga): implement.
	return make([]label.Label, 0)
}

func (erlang *Resolver) Resolve(
	c *config.Config,
	ix *resolve.RuleIndex,
	rc *repo.RemoteCache,
	r *rule.Rule,
	modulesRaw interface{},
	from label.Label,
) {
	if r.Kind() == erlangBytecodeKind || r.Kind() == appFileKind || r.Kind() == erlangAppInfoKind {
		original := r.AttrStrings("deps")
		if len(original) > 0 {
			resolved := make([]string, len(original))
			for i, d := range original {
				resolved[i] = fmt.Sprintf("@%s//:erlang_app", d)
			}
			// fmt.Println("Resolve:", r.Name, modulesRaw, from)
			r.SetAttr("deps", resolved)
		}
	}
}
