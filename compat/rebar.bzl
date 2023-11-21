load("@rules_erlang//:erlc_opts_file.bzl", "erlc_opts_file")

def erlc_opts_from_rebar_config(
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
