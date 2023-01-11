package erlang

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/repo"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

const languageName = "erlang"

var resolveableKinds = NewMutableSet(
	erlangBytecodeKind,
	erlangAppKind,
	testErlangAppKind,
	pltKind,
	ctTestKind,
)

type Resolver struct{}

// This won't get called for rules in the app.bzl macro file, at least
// not automatically...
func (erlang *Resolver) Imports(c *config.Config, r *rule.Rule, f *rule.File) []resolve.ImportSpec {
	Log(c, "Imports:", f.Path, r.Name(), "/", r.Kind())
	var specs []resolve.ImportSpec
	if resolveableKinds.Contains(r.Kind()) {
		for _, dep := range r.AttrStrings("deps") {
			if !strings.Contains(dep, ":") {
				specs = append(specs, resolve.ImportSpec{
					Lang: languageName,
					Imp:  dep,
				})
			}
		}
	}
	Log(c, "    Specs:", specs)
	return specs
}

func (erlang *Resolver) Embeds(r *rule.Rule, from label.Label) []label.Label {
	// fmt.Println("Embeds:", r.Name)
	return make([]label.Label, 0)
}

func appsDirApps(c *config.Config, rel string) map[string]string {
	result := make(map[string]string)
	erlangConfig := erlangConfigForRel(c, rel)
	for _, appsDir := range erlangConfig.AppsDirs.Values(strings.Compare) {
		absDir := filepath.Join(c.RepoRoot, appsDir)
		if _, err := os.Stat(absDir); !os.IsNotExist(err) {
			dirs, err := ioutil.ReadDir(absDir)
			if err != nil {
				log.Fatal(err)
			}
			for _, d := range dirs {
				if d.IsDir() {
					result[d.Name()] = filepath.Join(appsDir, d.Name())
				}
			}
		}
	}
	return result
}

func resolveErlangDeps(c *config.Config, rel string, r *rule.Rule) {
	apps := appsDirApps(c, rel)

	originals := r.AttrStrings("deps")
	if len(originals) > 0 {
		resolved := make([]string, len(originals))
		for i, dep := range originals {
			if strings.Contains(dep, ":") {
				resolved[i] = dep
			} else {
				if pkg, ok := apps[dep]; ok {
					resolved[i] = fmt.Sprintf("//%s:erlang_app", pkg)
				} else {
					resolved[i] = fmt.Sprintf("@%s//:erlang_app", dep)
				}
				Log(c, "       ", dep, "->", resolved[i])
			}
		}
		r.SetAttr("deps", resolved)
	}
}

func (erlang *Resolver) Resolve(
	c *config.Config,
	ix *resolve.RuleIndex,
	rc *repo.RemoteCache,
	r *rule.Rule,
	imports interface{},
	from label.Label,
) {
	// Log(c, fmt.Sprintf("Resolve: %s:%s", from.Pkg, from.Name))
	if resolveableKinds.Contains(r.Kind()) {
		Log(c, fmt.Sprintf("Resolve: //%s:%s", from.Pkg, from.Name), imports)
		resolveErlangDeps(c, from.Pkg, r)
	}
}
