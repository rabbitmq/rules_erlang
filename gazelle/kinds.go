package erlang

import (
	"github.com/bazelbuild/bazel-gazelle/rule"
)

const (
	erlangBytecodeKind = "erlang_bytecode"
	appFileKind        = "app_file"
	erlangAppInfoKind  = "erlang_app_info"
	untarKind          = "untar"
	hexPmErlangAppKind = "hex_pm_erlang_app"
)

func (*erlangLang) Kinds() map[string]rule.KindInfo {
	return erlangKinds
}

var erlangKinds = map[string]rule.KindInfo{
	"alias": {
		NonEmptyAttrs:  map[string]bool{"actual": true},
		MergeableAttrs: map[string]bool{"actual": true},
	},
	erlangBytecodeKind: {
		MatchAttrs: []string{"srcs"},
		NonEmptyAttrs: map[string]bool{
			"deps":       true,
			"srcs":       true,
			"hdrs":       true,
			"dest":       false,
			"erlc_opts":  false,
			"outs":       true,
			"visibility": true,
		},
		SubstituteAttrs: map[string]bool{},
		MergeableAttrs: map[string]bool{
			"srcs": true,
			"hdrs": true,
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
}

func (erlang *erlangLang) Loads() []rule.LoadInfo {
	return erlangLoads
}

var erlangLoads = []rule.LoadInfo{
	{
		Name: "@rules_erlang//:erlang_bytecode2.bzl",
		Symbols: []string{
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
}
