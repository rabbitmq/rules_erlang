package erlang

import (
	"github.com/bazelbuild/bazel-gazelle/rule"
)

const (
	erlcOptsKind         = "erlc_opts"
	erlangBytecodeKind   = "erlang_bytecode"
	erlangAppKind        = "erlang_app"
	testErlangAppKind    = "test_erlang_app"
	assertSuitesKind     = "assert_suites2"
	untarKind            = "untar"
	hexPmErlangAppKind   = "hex_pm_erlang_app"
	githubErlangAppKind  = "github_erlang_app"
	allBeamFilesKind     = "all_beam_files"
	allTestBeamFilesKind = "all_test_beam_files"
	allSrcsKind          = "all_srcs"
)

func (*erlangLang) Kinds() map[string]rule.KindInfo {
	return erlangKinds
}

var erlangAppKindInfo = rule.KindInfo{
	MatchAny: true,
	NonEmptyAttrs: map[string]bool{
		"app_name":        true,
		"app_version":     true,
		"app_description": true,
		"extra_apps":      true,
		"visibility":      true,
	},
	SubstituteAttrs: map[string]bool{},
	MergeableAttrs: map[string]bool{
		"beam_files":          true,
		"public_hdrs":         true,
		"all_srcs":            true,
		"extra_priv":          true,
		"extra_license_files": true,
	},
	ResolveAttrs: map[string]bool{
		"deps": true,
	},
}

var erlangKinds = map[string]rule.KindInfo{
	"alias": {
		NonEmptyAttrs:  map[string]bool{"actual": true},
		MergeableAttrs: map[string]bool{"actual": true},
	},
	"filegroup": {
		MatchAttrs: []string{"srcs"},
		NonEmptyAttrs: map[string]bool{
			"srcs":       true,
			"visibility": true,
		},
		SubstituteAttrs: map[string]bool{},
		MergeableAttrs: map[string]bool{
			"srcs": true,
		},
		ResolveAttrs: map[string]bool{},
	},
	allBeamFilesKind: {
		MatchAny: true,
	},
	allTestBeamFilesKind: {
		MatchAny: true,
	},
	allSrcsKind: {
		MatchAny: true,
	},
	erlcOptsKind: {
		MatchAttrs: []string{"values"},
		NonEmptyAttrs: map[string]bool{
			"values":     true,
			"visibility": true,
		},
		SubstituteAttrs: map[string]bool{},
		MergeableAttrs:  map[string]bool{},
		ResolveAttrs:    map[string]bool{},
	},
	erlangBytecodeKind: {
		MatchAttrs: []string{"srcs"},
		NonEmptyAttrs: map[string]bool{
			"beam":       true,
			"deps":       true,
			"srcs":       true,
			"hdrs":       true,
			"outs":       true,
			"visibility": true,
		},
		SubstituteAttrs: map[string]bool{},
		MergeableAttrs: map[string]bool{
			"srcs": true,
			"hdrs": true,
			"beam": true,
			"outs": true,
		},
		ResolveAttrs: map[string]bool{
			"deps": true,
		},
	},
	erlangAppKind:     erlangAppKindInfo,
	testErlangAppKind: erlangAppKindInfo,
	assertSuitesKind: {
		MatchAny: true,
		NonEmptyAttrs: map[string]bool{
			"suite_files": true,
		},
		SubstituteAttrs: map[string]bool{},
		MergeableAttrs:  map[string]bool{},
		ResolveAttrs:    map[string]bool{},
	},
	untarKind: {
		MatchAttrs: []string{"archive"},
		NonEmptyAttrs: map[string]bool{
			"archive":    true,
			"outs":       true,
			"visibility": true,
		},
		SubstituteAttrs: map[string]bool{},
		MergeableAttrs: map[string]bool{
			"outs": true,
		},
		ResolveAttrs: map[string]bool{},
	},
	hexPmErlangAppKind: {
		NonEmptyAttrs: map[string]bool{
			"version": true,
		},
		SubstituteAttrs: map[string]bool{},
		MergeableAttrs: map[string]bool{
			"version": true,
		},
	},
	githubErlangAppKind: {
		NonEmptyAttrs: map[string]bool{
			"repo":    true,
			"org":     true,
			"ref":     true,
			"version": true,
		},
		SubstituteAttrs: map[string]bool{},
		MergeableAttrs: map[string]bool{
			"ref":     true,
			"version": true,
		},
	},
}

func (erlang *erlangLang) Loads() []rule.LoadInfo {
	return erlangLoads
}

var erlangLoads = []rule.LoadInfo{
	{
		Name: "@rules_erlang//:erlang_bytecode2.bzl",
		Symbols: []string{
			erlcOptsKind,
			erlangBytecodeKind,
		},
	},
	{
		Name: "@rules_erlang//:erlang_app.bzl",
		Symbols: []string{
			erlangAppKind,
			testErlangAppKind,
		},
	},
	{
		Name: "@rules_erlang//:ct.bzl",
		Symbols: []string{
			assertSuitesKind,
		},
	},
	{
		Name: "@rules_erlang//:untar.bzl",
		Symbols: []string{
			untarKind,
		},
	},
	{
		Name: "@rules_erlang//:hex_pm.bzl",
		Symbols: []string{
			hexPmErlangAppKind,
		},
	},
	{
		Name: "@rules_erlang//:github.bzl",
		Symbols: []string{
			githubErlangAppKind,
		},
	},
	{
		Name: ":app.bzl",
		Symbols: []string{
			allBeamFilesKind,
			allTestBeamFilesKind,
			allSrcsKind,
		},
	},
}
