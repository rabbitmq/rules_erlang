package erlang_test

import (
	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/language"
	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
	erlang "github.com/rabbitmq/rules_erlang/gazelle"
	"testing"
)

func TestUpdate(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Erlang Suite")
}

var _ = Describe("an ErlangApp", func() {
	var c *config.Config
	var args language.GenerateArgs
	var app *erlang.ErlangApp

	BeforeEach(func() {
		configurer := erlang.Configurer{}
		c = config.New()
		configurer.CheckFlags(nil, c)

		args = language.GenerateArgs{
			Config: c,
		}

		app = erlang.NewErlangApp(args.Config.RepoRoot, args.Rel)
	})

	Describe("BeamFilesRules", func() {
		BeforeEach(func() {
			app.AddFile("src/foo.erl")
			app.AddFile("src/foo.hrl")
		})

		It("include_lib behaves like include if a file simply exists", func() {
			fakeParser := fakeErlParser(map[string]*erlang.ErlAttrs{
				"src/foo.erl": &erlang.ErlAttrs{
					IncludeLib: []string{"foo.hrl"},
				},
			})

			rules := app.BeamFilesRules(args, fakeParser)
			Expect(rules).NotTo(BeEmpty())

			Expect(rules[0].Name()).To(Equal("ebin_foo_beam"))
			Expect(rules[0].AttrStrings("hdrs")).To(ContainElements("src/foo.hrl"))
		})
	})
})
