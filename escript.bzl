load("//private:erlang_bytecode.bzl", "erlang_bytecode")
load(
    "//private:escript_beam.bzl",
    _escript_beam = "escript_beam",
)

def escript_beam(
        name = "",
        beam = [],
        out = "",
        erlang_label = DEFAULT_LABEL):
    _escript_beam(
        name = name,
        erl = Label("//tools:erl-{}".format(erlang_label)),
        beam = beam,
        out = out,
    )
