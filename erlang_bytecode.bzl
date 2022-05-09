load(
    "//private:erlang_bytecode.bzl",
    _erlang_bytecode = "erlang_bytecode",
)
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_INSTALLATION",
)
load(
    "//tools/compile_first:compile_first.bzl",
    "DEFAULT_COMPILE_FIRST",
)

def erlang_bytecode(
        erlang_installation = DEFAULT_ERLANG_INSTALLATION,
        compile_first = DEFAULT_COMPILE_FIRST,
        **kwargs):
    _erlang_bytecode(
        erlang_installation = erlang_installation,
        compile_first = compile_first,
        **kwargs
    )
