load(
    "//private:erlang_bytecode2.bzl",
    _ErlcOptsInfo = "ErlcOptsInfo",
    _erlc_opts = "erlc_opts",
)
load(
    "//private:erlang_bytecode_pw.bzl",
    _erlang_bytecode = "erlang_bytecode",
)

ErlcOptsInfo = _ErlcOptsInfo

def erlang_bytecode(
        srcs = [],
        outs = None,
        dest = None,
        erlc_worker = Label("//tools/erlc_persistent_worker:erlc_persistent_worker_wrapper"),
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
        erlc_worker = erlc_worker,
        **kwargs
    )

def erlc_opts(**kwargs):
    _erlc_opts(**kwargs)

def _beam(p):
    (_, _, basename) = p.rpartition("/")
    return basename.removesuffix(".erl") + ".beam"