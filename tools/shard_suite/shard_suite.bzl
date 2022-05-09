load("//private:erlang_bytecode.bzl", "erlang_bytecode")
load("//private:escript_flat.bzl", "escript_flat")
load("//tools:erlang.bzl", "DEFAULT_VERSION")

DEFAULT_SHARD_SUITE = "@otp_{}//:shard_suite".format(DEFAULT_VERSION)

def shard_suite(name_suffix = ""):
    erlang_bytecode(
        name = "shard_suite_beam{}".format(name_suffix),
        dest = "alt",
        erlang_installation = ":otp{}".format(name_suffix),
        srcs = [
            Label("//tools/shard_suite:src/shard_suite.erl"),
        ],
    )

    escript_flat(
        name = "shard_suite{}".format(name_suffix),
        erlang_installation = ":otp{}".format(name_suffix),
        beam = ["shard_suite_beam{}".format(name_suffix)],
        visibility = ["//visibility:public"],
    )
