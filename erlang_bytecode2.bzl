load(
    "//private:erlang_bytecode2.bzl",
    _erlang_bytecode = "erlang_bytecode",
)

def erlang_bytecode(
        srcs = [],
        outs = None,
        dest = None,
        **kwargs):
    if len(srcs) == 0:
        fail("srcs cannot be empty")
    if outs != None and dest != None:
        fail('"outs" and "dest" cannot be set together')
    if outs == None:
        if dest == None:
            fail('either "outs" or "dest" must be set')
        outs = [
            dest + "/" + _beam(src)
            for src in srcs
        ]

    _erlang_bytecode(
        srcs = srcs,
        outs = outs,
        **kwargs
    )

def _beam(p):
    (_, _, basename) = p.rpartition("/")
    return basename.removesuffix(".erl") + ".beam"
