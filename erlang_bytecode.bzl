load(
    "//private:erlang_bytecode.bzl",
    _erlang_bytecode = "erlang_bytecode",
)
load("//tools:erlang.bzl", "DEFAULT_LABEL")

def erlang_bytecode(
        erlang_version_label = DEFAULT_LABEL,
        **kwargs):
    _erlang_bytecode(
        erlang_installation = Label("//tools:otp-{}-installation".format(erlang_version_label)),
        compile_first = Label("//tools/compile_first:escript-{}".format(erlang_version_label)),
        **kwargs
    )
