package erlang

import (
	"github.com/bazelbuild/bazel-gazelle/rule"
)

const (
	erlcOptsKind        = "erlc_opts"
	erlangBytecodeKind  = "erlang_bytecode"
	appFileKind         = "app_file"
	erlangAppInfoKind   = "erlang_app_info"
	untarKind           = "untar"
	hexPmErlangAppKind  = "hex_pm_erlang_app"
	githubErlangAppKind = "github_erlang_app"
)

func (*erlangLang) Kinds() map[string]rule.KindInfo {
	return erlangKinds
}

var erlangKinds = map[string]rule.KindInfo{
	"alias": {
		NonEmptyAttrs:  map[string]bool{"actual": true},
		MergeableAttrs: map[string]bool{"actual": true},
	},
	erlcOptsKind: {
		MatchAny: true,
		NonEmptyAttrs: map[string]bool{
			"erlc_opts":  true,
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
			"erlc_opts":  false,
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
	appFileKind: {
		MatchAttrs: []string{"app_name"},
		NonEmptyAttrs: map[string]bool{
			"app_description": true,
			"app_name":        true,
			"app_src":         true,
			"app_version":     true,
			"deps":            true,
			"out":             true,
			"modules":         true,
			"stamp":           false,
			"visibility":      true,
		},
		SubstituteAttrs: map[string]bool{},
		MergeableAttrs:  map[string]bool{},
		ResolveAttrs: map[string]bool{
			"deps": true,
		},
	},
	erlangAppInfoKind: {
		MatchAttrs: []string{"app_name"},
		NonEmptyAttrs: map[string]bool{
			"srcs":          true,
			"hdrs":          true,
			"app":           true,
			"app_name":      true,
			"extra_apps":    true,
			"beam":          true,
			"license_files": true,
			"visibility":    true,
		},
		SubstituteAttrs: map[string]bool{},
		MergeableAttrs: map[string]bool{
			"srcs": true,
			"hdrs": true,
		},
		ResolveAttrs: map[string]bool{
			"deps": true,
		},
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
		Name: "@rules_erlang//:app_file2.bzl",
		Symbols: []string{
			appFileKind,
		},
	},
	{
		Name: "@rules_erlang//:erlang_app_info.bzl",
		Symbols: []string{
			erlangAppInfoKind,
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
}
