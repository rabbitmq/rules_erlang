load(
    "//private:erlang_build.bzl",
    "erlang_build",
)
load(
    ":erlang_toolchain.bzl",
    "erlang_toolchain",
)

DEFAULT_ERLANG_VERSION = "24.3.3"
DEFAULT_ERLANG_MAJOR = 24
DEFAULT_ERLANG_SHA256 = "cc3177f765c6a2b018e9a80c30bd3eac9a1f1d4c2690bb10557b384a9a63ae8d"
DEFAULT_ERLANG_INSTALLATION = "@otp_default//:erlang_installation"

ERLC_OPTS = [
    "-Werror",
    "+deterministic",
    "+warn_export_vars",
    "+warn_shadow_vars",
    "+warn_obsolete_guard",
]

def standard_erlang_tools(major_version = -1):
    erlang_build(
        name = "otp_linux",
        sources = native.glob(
            ["**/*"],
            exclude = ["BUILD.bazel", "WORKSPACE.bazel"],
        ),
        # extra_env = select({
        #     "@bazel_tools//src/conditions:darwin": {
        #         "CC": "clang",
        #     },
        #     "//conditions:default": {},
        # }),
        # extra_configure_opts = select({
        #     "@bazel_tools//src/conditions:darwin": [
        #         "--enable-darwin-64bit",
        #         # "--with-ssl=$(brew --prefix openssl@1.1)",
        #     ],
        #     "//conditions:default": [],
        # }),
    )

    erlang_toolchain(
        name = "erlang_linux",
        otp = ":otp_linux",
    )

    native.toolchain(
        name = "erlang_linux_toolchain",
        # exec_compatible_with = [
        #     "@platforms//os:linux",
        #     # "@platforms//cpu:x86_64",
        # ],
        target_compatible_with = [
            # "@platforms//os:linux",
            # "@platforms//cpu:x86_64",
            "@rules_erlang//:erlang_{}".format(major_version),
        ],
        toolchain = ":erlang_linux",
        toolchain_type = "@rules_erlang//tools:toolchain_type",
        visibility = ["//visibility:public"],
    )
