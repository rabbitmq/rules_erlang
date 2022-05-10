load(
    "//private:erlang_bytecode.bzl",
    _erlang_bytecode = "erlang_bytecode",
)
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_INSTALLATION",
)

def erlang_bytecode(
        erlang_installation = DEFAULT_ERLANG_INSTALLATION,
        **kwargs):
    _erlang_bytecode(
        erlang_installation = erlang_installation,
        **kwargs
    )
