load(":app_file.bzl", "app_file")
load(":erlang_bytecode.bzl", "erlang_bytecode")
load(
    ":erlang_app_info.bzl",
    "erlang_app_info",
    _ErlangAppInfo = "ErlangAppInfo",
)
load(
    ":util.bzl",
    "without",
)

DEFAULT_ERLC_OPTS = [
    "-Werror",
    "+deterministic",
    "+debug_info",
    "+warn_export_vars",
    "+warn_shadow_vars",
    "+warn_obsolete_guard",
]

DEFAULT_TEST_ERLC_OPTS = [
    "+deterministic",
    "+debug_info",
    "+warn_export_vars",
    "+warn_shadow_vars",
    "+warn_obsolete_guard",
    "-DTEST=1",
]

ErlangAppInfo = _ErlangAppInfo

def _erlang_app(
        app_name = "",
        app_version = "",
        app_description = "",
        app_module = "",
        app_registered = [],
        app_env = "",
        app_extra_keys = "",
        extra_apps = [],
        erlc_opts = [],
        extra_hdrs = [],
        extra_srcs = [],
        extra_priv = [],
        extra_license_files = [],
        build_deps = [],
        deps = [],
        runtime_deps = [],
        stamp = None,
        # new attrs for gazelle extension
        beam_files = None,
        public_hdrs = None,
        all_srcs = None,
        test = False):
    if beam_files != None or public_hdrs != None or all_srcs != None:
        if len(erlc_opts) > 0:
            fail("Cannot set beam_files, public_hdrs or all_srcs AND erlc_opts")
        if len(extra_hdrs) > 0:
            fail("Cannot set beam_files, public_hdrs or all_srcs AND extra_hdrs")
        if len(extra_srcs) > 0:
            fail("Cannot set beam_files, public_hdrs or all_srcs AND extra_srcs")
        if len(build_deps) > 0:
            print("Warning: build_deps are ignored when beam_files is set")
        if len(runtime_deps) > 0:
            print("Warning: deps and runtime_deps are equivalent when beam_files is set, consider just using deps")

    if stamp == None:
        stamp = -1 if not test else 0

    if beam_files == None:
        if not test:
            erlc_opts = select({
                Label("@rules_erlang//:debug_build"): without("+deterministic", DEFAULT_ERLC_OPTS),
                "//conditions:default": DEFAULT_ERLC_OPTS,
            })
        else:
            erlc_opts = select({
                Label("@rules_erlang//:debug_build"): without("+deterministic", DEFAULT_TEST_ERLC_OPTS),
                "//conditions:default": DEFAULT_TEST_ERLC_OPTS,
            })
        srcs = native.glob(
            ["src/**/*.erl"],
            exclude = extra_srcs,
        ) + extra_srcs
        hdrs = native.glob(
            ["include/**/*.hrl", "src/**/*.hrl"],
            exclude = extra_hdrs,
        ) + extra_hdrs
        public_hdrs = native.glob(
            ["include/**/*.hrl"],
            exclude = extra_hdrs,
        ) + extra_hdrs
        all_srcs = hdrs + srcs

        erlang_bytecode(
            name = "beam_files" if not test else "test_beam_files",
            app_name = app_name,
            hdrs = hdrs,
            srcs = srcs,
            erlc_opts = erlc_opts,
            dest = "ebin" if not test else "test",
            deps = build_deps + deps,
            testonly = test,
        )

        beam_files = [":beam_files" if not test else ":test_beam_files"]

    if len(native.glob(["ebin/{}.app".format(app_name)])) == 0:
        if not test:
            app_file(
                name = "app_file",
                app_name = app_name,
                app_version = app_version,
                app_description = app_description,
                app_module = app_module,
                app_registered = app_registered,
                app_env = app_env,
                app_extra_keys = app_extra_keys,
                extra_apps = extra_apps,
                app_src = native.glob(["src/{}.app.src".format(app_name)]),
                modules = beam_files,
                deps = deps + runtime_deps,
                dest = "ebin",
                stamp = stamp,
            )
        app = ":app_file"
    else:
        app = "ebin/{}.app".format(app_name)

    erlang_app_info(
        name = "erlang_app" if not test else "test_erlang_app",
        app_name = app_name,
        hdrs = public_hdrs,
        app = app,
        beam = beam_files,
        priv = native.glob(["priv/**/*"]) + extra_priv,
        license_files = native.glob(
            ["LICENSE*"],
            exclude = extra_license_files,
        ) + extra_license_files,
        srcs = all_srcs,
        deps = deps + runtime_deps,
        visibility = ["//visibility:public"],
        testonly = test,
    )

def erlang_app(**kwargs):
    _erlang_app(
        test = False,
        **kwargs
    )

def test_erlang_app(**kwargs):
    _erlang_app(
        test = True,
        **kwargs
    )
