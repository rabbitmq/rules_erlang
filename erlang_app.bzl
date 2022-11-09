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

def erlang_app(
        app_name = "",
        app_version = "",
        app_description = "",
        app_module = "",
        app_registered = [],
        app_env = "",
        app_extra_keys = "",
        extra_apps = [],
        erlc_opts = select({
            Label("@rules_erlang//:debug_build"): without("+deterministic", DEFAULT_ERLC_OPTS),
            "//conditions:default": DEFAULT_ERLC_OPTS,
        }),
        extra_hdrs = [],
        extra_srcs = [],
        extra_priv = [],
        extra_license_files = [],
        build_deps = [],
        deps = [],
        runtime_deps = [],
        stamp = -1):
    srcs = native.glob(
        ["src/**/*.erl"],
        exclude = extra_srcs,
    ) + extra_srcs
    hdrs = native.glob(
        ["include/**/*.hrl", "src/**/*.hrl"],
        exclude = extra_hdrs,
    ) + extra_hdrs

    erlang_bytecode(
        name = "beam_files",
        app_name = app_name,
        hdrs = hdrs,
        srcs = srcs,
        erlc_opts = erlc_opts,
        dest = "ebin",
        deps = build_deps + deps,
    )

    if len(native.glob(["ebin/{}.app".format(app_name)])) == 0:
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
            modules = [":beam_files"],
            deps = deps + runtime_deps,
            dest = "ebin",
            stamp = stamp,
        )
        app = ":app_file"
    else:
        app = "ebin/{}.app".format(app_name)

    erlang_app_info(
        name = app_name,
        app_name = app_name,
        hdrs = native.glob(
            ["include/**/*.hrl"],
            exclude = extra_hdrs,
        ) + extra_hdrs,
        app = app,
        beam = [":beam_files"],
        priv = native.glob(["priv/**/*"]) + extra_priv,
        license_files = native.glob(
            ["LICENSE*"],
            exclude = extra_license_files,
        ) + extra_license_files,
        srcs = hdrs + srcs,
        deps = deps + runtime_deps,
        visibility = ["//visibility:public"],
    )

    native.alias(
        name = "erlang_app",
        actual = ":%s" % app_name,
        visibility = ["//visibility:public"],
    )

def test_erlang_app(
        app_name = "",
        app_version = "",
        app_description = "",
        app_module = "",
        app_registered = [],
        app_env = "",
        app_extra_keys = "",
        extra_apps = [],
        erlc_opts = select({
            Label("@rules_erlang//:debug_build"): without("+deterministic", DEFAULT_TEST_ERLC_OPTS),
            "//conditions:default": DEFAULT_TEST_ERLC_OPTS,
        }),
        extra_hdrs = [],
        extra_srcs = [],
        extra_priv = [],
        extra_license_files = [],
        build_deps = [],
        deps = [],
        runtime_deps = []):
    srcs = native.glob(
        ["src/**/*.erl"],
        exclude = extra_srcs,
    ) + extra_srcs
    hdrs = native.glob(
        ["include/**/*.hrl", "src/**/*.hrl"],
        exclude = extra_hdrs,
    ) + extra_hdrs

    erlang_bytecode(
        name = "test_beam_files",
        app_name = app_name,
        hdrs = hdrs,
        srcs = srcs,
        erlc_opts = erlc_opts,
        dest = "test",
        deps = build_deps + deps,
        testonly = True,
    )

    if len(native.glob(["ebin/{}.app".format(app_name)])) == 0:
        app = ":app_file"
    else:
        app = "ebin/{}.app".format(app_name)

    erlang_app_info(
        name = "test_erlang_app",
        app_name = app_name,
        hdrs = hdrs,
        app = app,
        beam = [":test_beam_files"],
        priv = native.glob(["priv/**/*"]) + extra_priv,
        license_files = native.glob(
            ["LICENSE*"],
            exclude = extra_license_files,
        ) + extra_license_files,
        srcs = hdrs + srcs,
        deps = deps + runtime_deps,
        visibility = ["//visibility:public"],
        testonly = True,
    )
