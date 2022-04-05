load(":erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load(":app_file.bzl", "app_file")
load(":erlc.bzl", "erlc")
load(":util.bzl", "path_join")
load(
    ":erlang_app_info.bzl",
    "erlang_app_info",
    _ErlangAppInfo = "ErlangAppInfo",
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
        app_extra_keys = [],
        extra_apps = [],
        erlc_opts = DEFAULT_ERLC_OPTS,
        first_srcs = [],
        extra_priv = [],
        extra_license_files = [],
        build_deps = [],
        deps = [],
        runtime_deps = [],
        stamp = -1):
    all_beam = []

    if len(first_srcs) > 0:
        all_beam = [":first_beam_files"]
        erlc(
            name = "first_beam_files",
            hdrs = native.glob(["include/**/*.hrl", "src/**/*.hrl"]),
            srcs = native.glob(first_srcs),
            erlc_opts = erlc_opts,
            dest = "ebin",
            deps = build_deps + deps,
        )

    erlc(
        name = "beam_files",
        hdrs = native.glob(["include/**/*.hrl", "src/**/*.hrl"]),
        srcs = native.glob(["src/**/*.erl"], exclude = first_srcs),
        beam = all_beam,
        erlc_opts = erlc_opts,
        dest = "ebin",
        deps = build_deps + deps,
    )

    all_beam = all_beam + [":beam_files"]

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
            modules = all_beam,
            deps = deps + runtime_deps,
            stamp = stamp,
        )
        app = ":app_file"
    else:
        app = "ebin/{}.app".format(app_name)

    erlang_app_info(
        name = "erlang_app",
        app_name = app_name,
        hdrs = native.glob(["include/**/*.hrl"]),
        app = app,
        beam = all_beam,
        priv = native.glob(["priv/**/*"]) + extra_priv,
        license_files = native.glob(["LICENSE*"]) + extra_license_files,
        deps = deps + runtime_deps,
        visibility = ["//visibility:public"],
    )

def test_erlang_app(
        app_name = "",
        app_version = "",
        app_description = "",
        app_module = "",
        app_registered = [],
        app_env = "",
        app_extra_keys = [],
        extra_apps = [],
        erlc_opts = DEFAULT_TEST_ERLC_OPTS,
        first_srcs = [],
        extra_priv = [],
        extra_license_files = [],
        build_deps = [],
        deps = [],
        runtime_deps = []):
    all_test_beam = []

    if len(first_srcs) > 0:
        all_test_beam = [":first_test_beam_files"]
        erlc(
            name = "first_test_beam_files",
            hdrs = native.glob(["include/**/*.hrl", "src/**/*.hrl"]),
            srcs = native.glob(first_srcs),
            erlc_opts = erlc_opts,
            dest = "test",
            deps = build_deps + deps,
            testonly = True,
        )

    erlc(
        name = "test_beam_files",
        hdrs = native.glob(["include/**/*.hrl", "src/**/*.hrl"]),
        srcs = native.glob(["src/**/*.erl"], exclude = first_srcs),
        beam = all_test_beam,
        erlc_opts = erlc_opts,
        dest = "test",
        deps = build_deps + deps,
        testonly = True,
    )

    all_test_beam = all_test_beam + [":test_beam_files"]

    if len(native.glob(["ebin/{}.app".format(app_name)])) == 0:
        app = ":app_file"
    else:
        app = "ebin/{}.app".format(app_name)

    erlang_app_info(
        name = "test_erlang_app",
        app_name = app_name,
        hdrs = native.glob(["include/**/*.hrl", "src/**/*.hrl"]),
        app = app,
        beam = all_test_beam,
        priv = native.glob(["priv/**/*"]) + extra_priv,
        license_files = native.glob(["LICENSE*"]) + extra_license_files,
        deps = deps + runtime_deps,
        visibility = ["//visibility:public"],
        testonly = True,
    )
