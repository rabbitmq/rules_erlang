load(
    "@rules_erlang//compat:erlang_mk.bzl",
    "app_src_from_erlang_mk_makefile",
    "erlc_opts_from_erlang_mk_makefile",
)
load("@rules_erlang//compat:rebar.bzl", "erlc_opts_from_rebar_config")
load("@rules_erlang//:erlc_opts_file.bzl", "erlc_opts_file")
load("@rules_erlang//:erlang_app_sources.bzl", "erlang_app_sources")

def erlang_autodetect(name = None, testonly = False):
    app_src = None
    app_src_paths = native.glob(["src/%s.app.src" % name])
    if len(app_src_paths) == 1:
        app_src = app_src_paths[0]

    if len(native.glob(["Makefile", "erlang.mk"])) == 2:
        erlc_opts_from_erlang_mk_makefile(
            name = "erlc_opts_file",
            srcs = native.glob([
                "**/*",
            ]),
            out = "erlc_opts",
        )
        if app_src == None:
            app_src_from_erlang_mk_makefile(
                name = "app_src",
                srcs = native.glob([
                    "**/*",
                ]),
                out = "src/%s.app.src" % name,
            )
            app_src = ":app_src"
    elif len(native.glob(["rebar.config"])) == 1:
        erlc_opts_from_rebar_config(
            name = "erlc_opts_file",
            out = "erlc_opts",
        )
    else:
        erlc_opts_file(
            name = "erlc_opts_file",
            values = select({
                "@rules_erlang//:debug_build": ["+debug_info"],
                "//conditions:default": ["+deterministic", "+debug_info"],
            }),
            out = "erlc_opts",
        )

    erlang_app_sources(
        name = "srcs",
        app_name = name,
        app_src = app_src,
        erlc_opts_file = ":erlc_opts_file",
        testonly = testonly,
        visibility = ["//visibility:public"],
    )
