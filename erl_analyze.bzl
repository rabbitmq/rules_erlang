load(
    "//private:erl_analyze.bzl",
    _erl_analyze = "erl_analyze",
)
load(
    ":util.bzl",
    "path_join",
)

def erl_analyze(
        name = "analyze",
        hdrs = [],
        srcs = [],
        dest = "src",
        **kwargs):
    for src in srcs:
        module_name = _module_name(src)

        _erl_analyze(
            name = "{}_{}".format(name, module_name),
            hdrs = hdrs,
            src = src,
            out = path_join(dest, module_name + ".json"),
            erl_attrs_to_json_worker = Label("@rules_erlang//tools/erl_attrs_to_json_worker:wrapper"),
            **kwargs
        )

    native.filegroup(
        name = name,
        srcs = srcs,
    )

def _module_name(p):
    (_, _, basename) = p.rpartition("/")
    return basename.removesuffix(".erl")
