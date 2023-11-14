load(
    "//private:erl_analyze.bzl",
    _erl_analyze = "erl_analyze",
)

def erl_analyze(
        name = "analyze",
        **kwargs):

    _erl_analyze(
        name = name,
        erl_attrs_to_json_worker = Label("@rules_erlang//tools/erl_attrs_to_json_worker:wrapper"),
        **kwargs
    )
