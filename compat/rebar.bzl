load(
    "//compat/private:erlc_opts_from_rebar_config.bzl",
    _erlc_opts_from_rebar_config = "erlc_opts_from_rebar_config",
)

def erlc_opts_from_rebar_config(
    src = None,
    **kwargs):
    if src == None:
        if len(native.glob(["rebar.config"])) == 1:
            src = "rebar.config"
        else:
            fail("rebar.config file not found")

    _erlc_opts_from_rebar_config(
        src = src,
        extractor = Label("@rules_erlang//tools/rebar_config_to_erlc_opts:wrapper"),
        **kwargs)
