package erlang_test

import (
	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
	"github.com/bazelbuild/buildtools/build"
	. "github.com/onsi/ginkgo/v2"
	. "github.com/onsi/gomega"
	erlang "github.com/rabbitmq/rules_erlang/gazelle"
	"strings"
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

	Describe("AddFile", func() {
		BeforeEach(func() {
			app.AddFile("src/foo.app.src")
			app.AddFile("src/foo.erl")
			app.AddFile("src/foo.hrl")
			app.AddFile("include/bar.hrl")
			app.AddFile("test/foo_SUITE.erl")
			app.AddFile("priv/foo.img")
			app.AddFile("LICENSE")
			app.AddFile("src/bar.png")
		})

		It("puts .app.src files in AppSrc", func() {
			Expect(app.AppSrc).To(HaveLen(1))
			Expect(app.AppSrc.Contains("src/foo.app.src")).To(BeTrue())
		})

		It("puts src files in Srcs", func() {
			Expect(app.Srcs).To(HaveLen(1))
			Expect(app.Srcs.Contains("src/foo.erl")).To(BeTrue())
		})

		It("puts private hdrs in PrivateHdrs", func() {
			Expect(app.PrivateHdrs).To(HaveLen(1))
			Expect(app.PrivateHdrs.Contains("src/foo.hrl")).To(BeTrue())
		})

		It("puts public hdrs in PublicHdrs", func() {
			Expect(app.PublicHdrs).To(HaveLen(1))
			Expect(app.PublicHdrs.Contains("include/bar.hrl")).To(BeTrue())
		})

		It("puts test srcs in TestSrcs", func() {
			Expect(app.TestSrcs).To(HaveLen(1))
			Expect(app.TestSrcs.Contains("test/foo_SUITE.erl")).To(BeTrue())
		})

		It("puts priv in Priv", func() {
			Expect(app.Priv).To(HaveLen(1))
			Expect(app.Priv.Contains("priv/foo.img")).To(BeTrue())
		})

		It("puts license files in LicenseFiles", func() {
			Expect(app.LicenseFiles).To(HaveLen(1))
			Expect(app.LicenseFiles.Contains("LICENSE")).To(BeTrue())
		})
	})

	Describe("ErlcOptsRule", func() {
		BeforeEach(func() {
			app.ErlcOpts.Add("+warn_export_all")
		})

		It("wraps the opts with a select based on the :debug_build setting", func() {
			r := app.ErlcOptsRule()
			Expect(r.Name()).To(Equal("erlc_opts"))
			values := build.FormatString(r.Attr("values"))
			Expect(values).To(ContainSubstring("select("))
			Expect(values).To(ContainSubstring("+deterministic"))
			Expect(values).To(ContainSubstring("+warn_export_all"))
		})
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
			Expect(rules[0].AttrStrings("hdrs")).To(
				ContainElements("src/foo.hrl"))
		})

		It("resolves parse_transforms", func() {
			app.AddFile("src/bar.erl")

			fakeParser := fakeErlParser(map[string]*erlang.ErlAttrs{
				"src/foo.erl": &erlang.ErlAttrs{
					ParseTransform: []string{"bar"},
				},
			})

			rules := app.BeamFilesRules(args, fakeParser)
			Expect(rules).NotTo(BeEmpty())

			Expect(rules[0].Name()).To(Equal("ebin_bar_beam"))

			Expect(rules[1].Name()).To(Equal("ebin_foo_beam"))
			Expect(rules[1].AttrStrings("beam")).To(
				ContainElements("ebin/bar.beam"))
		})

		It("honors erlang_module_source_lib directives", func() {
			fakeParser := fakeErlParser(map[string]*erlang.ErlAttrs{
				"src/foo.erl": &erlang.ErlAttrs{
					ParseTransform: []string{"baz"},
					Call: map[string][]string{
						"fuzz": []string{"create"},
					},
				},
			})

			erlangConfigs := args.Config.Exts["erlang"].(erlang.ErlangConfigs)
			erlangConfig := erlangConfigs[args.Rel]
			erlangConfig.ModuleMappings["baz"] = "baz_app"
			erlangConfig.ModuleMappings["fuzz"] = "fuzz_app"

			rules := app.BeamFilesRules(args, fakeParser)
			Expect(rules).NotTo(BeEmpty())

			Expect(rules[0].Name()).To(Equal("ebin_foo_beam"))
			Expect(rules[0].AttrStrings("deps")).To(
				ContainElements("baz_app"))

			Expect(app.Deps.Values(strings.Compare)).To(
				ContainElements("fuzz_app"))
		})
	})

	Describe("Tests Rules", func() {
		var fakeParser erlang.ErlParser
		var testDirRules []*rule.Rule

		BeforeEach(func() {
			app.AddFile("src/foo.erl")
			app.AddFile("src/bar.erl")
			app.AddFile("test/foo_SUITE.erl")
			app.AddFile("test/foo_helper.erl")
			app.AddFile("test/bar_tests.erl")

			fakeParser = fakeErlParser(map[string]*erlang.ErlAttrs{
				"src/foo.erl": &erlang.ErlAttrs{},
				"test/foo_SUITE.erl": &erlang.ErlAttrs{
					ParseTransform: []string{"foo"},
					Call: map[string][]string{
						"foo_helper": []string{"make_test_thing"},
						"fuzz":       []string{"create"},
					},
				},
				"test/foo_helper.erl": &erlang.ErlAttrs{},
			})

			erlangConfigs := args.Config.Exts["erlang"].(erlang.ErlangConfigs)
			erlangConfig := erlangConfigs[args.Rel]
			erlangConfig.ModuleMappings["fuzz"] = "fuzz_app"

			testDirRules = app.TestDirBeamFilesRules(args, fakeParser)
		})

		Describe("TestDirBeamFilesRules", func() {
			It("Adds runtime deps to the suite", func() {
				Expect(testDirRules).To(HaveLen(3))

				Expect(testDirRules[0].Name()).To(Equal("test_bar_tests_beam"))

				Expect(testDirRules[1].Name()).To(Equal("foo_SUITE_beam_files"))
				Expect(testDirRules[1].AttrStrings("beam")).To(
					ContainElements("ebin/foo.beam"))

				Expect(testDirRules[2].Name()).To(Equal("test_foo_helper_beam"))
			})
		})

		Describe("EunitRule", func() {
			It("Adds runtime deps to the suite", func() {
				r := app.EunitRule()

				Expect(r.Name()).To(Equal("eunit"))
				Expect(r.AttrStrings("compiled_suites")).To(
					ContainElements(":test_foo_helper_beam"))
				Expect(r.AttrString("target")).To(Equal(":test_erlang_app"))
			})
		})

		Describe("CtSuiteRules", func() {
			It("Adds runtime deps to the suite", func() {
				rules := app.CtSuiteRules(testDirRules)
				Expect(rules).To(HaveLen(1))

				Expect(rules[0].Name()).To(Equal("foo_SUITE"))
				Expect(rules[0].AttrStrings("compiled_suites")).To(
					ContainElements(":foo_SUITE_beam_files", "test/foo_helper.beam"))
				Expect(rules[0].AttrStrings("deps")).To(
					ContainElements(":test_erlang_app", "fuzz_app"))
			})
		})
	})
})
