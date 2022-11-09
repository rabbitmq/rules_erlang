load(
    "//private:erlang_bytecode2.bzl",
    _ErlcOptsInfo = "ErlcOptsInfo",
    _erlang_bytecode = "erlang_bytecode",
    _erlc_opts = "erlc_opts",
)

ErlcOptsInfo = _ErlcOptsInfo

def erlang_bytecode(**kwargs):
    _erlang_bytecode(**kwargs)

def erlc_opts(**kwargs):
    _erlc_opts(**kwargs)
