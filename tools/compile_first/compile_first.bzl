load("//private:erlang_bytecode.bzl", "erlang_bytecode")
load("//private:escript_flat.bzl", "escript_flat")
load("//tools:erlang.bzl", "DEFAULT_VERSION")

DEFAULT_COMPILE_FIRST = "@otp_{}//:compile_first".format(DEFAULT_VERSION)

def compile_first(name_suffix = ""):
    erlang_bytecode(
        name = "compile_first_beam{}".format(name_suffix),
        erlang_installation = ":otp{}".format(name_suffix),
        srcs = [
            Label("//tools/compile_first:src/compile_first.erl"),
        ],
    )

    escript_flat(
        name = "compile_first{}".format(name_suffix),
        erlang_installation = ":otp{}".format(name_suffix),
        beam = ["compile_first_beam{}".format(name_suffix)],
        visibility = ["//visibility:public"],
    )
