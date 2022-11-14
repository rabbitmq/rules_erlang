package erlang

import (
	"fmt"
	"io/ioutil"
	"log"
	"path/filepath"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/repo"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

const languageName = "erlang"

type Resolver struct{}

func (erlang *Resolver) Imports(c *config.Config, r *rule.Rule, f *rule.File) []resolve.ImportSpec {
	// Log(c, "Imports:", f.Path)
	return nil
}

func (erlang *Resolver) Embeds(r *rule.Rule, from label.Label) []label.Label {
	// fmt.Println("Embeds:", r.Name)
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
	// Log(c, fmt.Sprintf("Resolve: %s from %s:%s", r.Name(), from.Pkg, from.Name))
	if r.Kind() == erlangBytecodeKind || r.Kind() == appFileKind || r.Kind() == erlangAppInfoKind {
		erlangConfig := erlangConfigForRel(c, from.Pkg)

		originals := r.AttrStrings("deps")
		if len(originals) > 0 {
			resolved := make([]string, len(originals))
			for i, dep := range originals {
				// TODO: cache this or generate it during config stage
				apps, err := ioutil.ReadDir(filepath.Join(c.RepoRoot, erlangConfig.AppsDir))
				if err != nil {
					log.Fatal(err)
				}

				for _, app := range apps {
					if app.Name() == dep {
						resolved[i] = fmt.Sprintf("//%s/%s:erlang_app", erlangConfig.AppsDir, dep)
					}
				}
				if resolved[i] == "" {
					resolved[i] = fmt.Sprintf("@%s//:erlang_app", dep)
				}

				Log(c, "    ", dep, "->", resolved[i])
			}
			r.SetAttr("deps", resolved)
		}
	}
}
