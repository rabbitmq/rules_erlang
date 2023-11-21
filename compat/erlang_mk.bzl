load("//:erlc_opts_file.bzl", "erlc_opts_file")
load(
    "//compat/private:app_src_from_erlang_mk_makefile.bzl",
    _app_src_from_erlang_mk_makefile = "app_src_from_erlang_mk_makefile",
)

def app_src_from_erlang_mk_makefile(**kwargs):
    _app_src_from_erlang_mk_makefile(**kwargs)

def erlc_opts_from_erlang_mk_makefile(
        name = None,
        **kwargs):
    erlc_opts_file(
        name = name,
        values = select({
            "@rules_erlang//:debug_build": ["+debug_info"],
            "//conditions:default": ["+deterministic", "+debug_info"],
        }),
        out = "erlc_opts",
    )
