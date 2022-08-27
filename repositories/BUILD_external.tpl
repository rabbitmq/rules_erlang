# This file is generated by rules_erlang via the erlang_config macro

load(
    "%{RULES_ERLANG_WORKSPACE}//private:erlang_build.bzl",
    "erlang_external",
)
load(
    "%{RULES_ERLANG_WORKSPACE}//tools:erlang_toolchain.bzl",
    "erlang_toolchain",
)

erlang_external(
    name = "otp",
    erl_path = %{ERL_PATH_VALUE},
)

erlang_toolchain(
    name = "erlang",
    otp = ":otp",
    visibility = ["//visibility:public"],
)

toolchain(
    name = "toolchain",
    target_compatible_with = [
        "//:erlang_external",
    ],
    toolchain = ":erlang",
    toolchain_type = "%{RULES_ERLANG_WORKSPACE}//tools:toolchain_type",
    visibility = ["//visibility:public"],
)
