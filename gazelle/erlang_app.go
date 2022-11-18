package erlang

import (
	"strings"

	"github.com/bazelbuild/bazel-gazelle/rule"
)

type erlangApp struct {
	Name         string
	Description  string
	Version      string
	Srcs         MutableSet[string]
	PrivateHdrs  MutableSet[string]
	PublicHdrs   MutableSet[string]
	AppSrc       MutableSet[string]
	TestSrcs     MutableSet[string]
	TestHdrs     MutableSet[string]
	LicenseFiles MutableSet[string]
	ErlcOpts     []string
	TestErlcOpts []string
	Deps         MutableSet[string]
	ExtraApps    MutableSet[string]
}

func newErlangApp() *erlangApp {
	return &erlangApp{
		Srcs:         NewMutableSet[string](),
		PrivateHdrs:  NewMutableSet[string](),
		PublicHdrs:   NewMutableSet[string](),
		AppSrc:       NewMutableSet[string](),
		TestSrcs:     NewMutableSet[string](),
		TestHdrs:     NewMutableSet[string](),
		LicenseFiles: NewMutableSet[string](),
		ErlcOpts:     []string{"+debug_info"},
		TestErlcOpts: []string{"+debug_info", "-DTEST=1"},
		Deps:         NewMutableSet[string](),
		ExtraApps:    NewMutableSet[string](),
	}
}

func (erlangApp *erlangApp) erlangAppRule(explicitFiles bool) *rule.Rule {
	r := rule.NewRule(erlangAppKind, "")
	r.SetAttr("app_name", erlangApp.Name)
	if erlangApp.Version != "" {
		r.SetAttr("app_version", erlangApp.Version)
	}
	if erlangApp.Description != "" {
		r.SetAttr("app_description", erlangApp.Description)
	}
	if !erlangApp.ExtraApps.IsEmpty() {
		r.SetAttr("extra_apps", erlangApp.ExtraApps.Values(strings.Compare))
	}
	r.SetAttr("erlc_opts", "//:"+erlcOptsRuleName)

	if explicitFiles {
		if !erlangApp.PublicHdrs.IsEmpty() {
			r.SetAttr("extra_hdrs", erlangApp.PublicHdrs.Values(strings.Compare))
		}
		r.SetAttr("extra_srcs", erlangApp.Srcs.Values(strings.Compare))

		if !erlangApp.LicenseFiles.IsEmpty() {
			r.SetAttr("extra_license_files", erlangApp.LicenseFiles.Values(strings.Compare))
		}
	}

	if !erlangApp.Deps.IsEmpty() {
		r.SetAttr("deps", erlangApp.Deps.Values(strings.Compare))
	}
	return r
}

func (erlangApp *erlangApp) testErlangAppRule(explicitFiles bool) *rule.Rule {
	r := rule.NewRule(testErlangAppKind, "")
	r.SetAttr("app_name", erlangApp.Name)
	if erlangApp.Version != "" {
		r.SetAttr("app_version", erlangApp.Version)
	}
	if erlangApp.Description != "" {
		r.SetAttr("app_description", erlangApp.Description)
	}
	if !erlangApp.ExtraApps.IsEmpty() {
		r.SetAttr("extra_apps", erlangApp.ExtraApps.Values(strings.Compare))
	}
	r.SetAttr("erlc_opts", "//:"+testErlcOptsRuleName)

	if explicitFiles {
		if !erlangApp.PublicHdrs.IsEmpty() {
			r.SetAttr("extra_hdrs", erlangApp.PublicHdrs.Values(strings.Compare))
		}
		r.SetAttr("extra_srcs", Union(erlangApp.Srcs, erlangApp.PrivateHdrs).Values(strings.Compare))

		if !erlangApp.LicenseFiles.IsEmpty() {
			r.SetAttr("extra_license_files", erlangApp.LicenseFiles.Values(strings.Compare))
		}
	}

	if !erlangApp.Deps.IsEmpty() {
		r.SetAttr("deps", erlangApp.Deps.Values(strings.Compare))
	}
	return r
}
