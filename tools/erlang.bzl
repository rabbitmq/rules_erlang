load(
    "//private:erlang_build.bzl",
    "erlang_build",
)
load(
    ":erlang_toolchain.bzl",
    "erlang_toolchain",
)

DEFAULT_ERLANG_VERSION = "24.3.3"
DEFAULT_ERLANG_SHA256 = "cc3177f765c6a2b018e9a80c30bd3eac9a1f1d4c2690bb10557b384a9a63ae8d"

ERLC_OPTS = [
    "-Werror",
    "+deterministic",
    "+warn_export_vars",
    "+warn_shadow_vars",
    "+warn_obsolete_guard",
]

def standard_erlang_tools(extra_configure_opts):
    erlang_build(
        name = "otp",
        sources = native.glob(
            ["**/*"],
            exclude = ["BUILD.bazel", "WORKSPACE.bazel"],
        ),
        extra_configure_opts = extra_configure_opts,
    )

    erlang_toolchain(
        name = "erlang",
        otp = ":otp",
    )

    native.toolchain(
        name = "erlang_toolchain",
        toolchain = ":erlang",
        toolchain_type = "@rules_erlang//tools:toolchain_type",
        visibility = ["//visibility:public"],
    )
