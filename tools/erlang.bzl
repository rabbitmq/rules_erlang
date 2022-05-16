load(
    "//private:erlang_build.bzl",
    "erlang_build",
)
load(
    "//private:erlang_bytecode.bzl",
    "erlang_bytecode",
)
load(
    "//private:erlang_tool.bzl",
    "erlang_tool",
)
load(
    "//private:escript_flat.bzl",
    "escript_flat",
)
load(
    ":erlang_installation.bzl",
    "erlang_installation",
)

DEFAULT_ERLANG_VERSION = "24.3.3"
DEFAULT_ERLANG_SHA256 = "cc3177f765c6a2b018e9a80c30bd3eac9a1f1d4c2690bb10557b384a9a63ae8d"
DEFAULT_ERLANG_INSTALLATION = "@otp_default//:erlang_installation"

ERLC_OPTS = [
    "-Werror",
    "+deterministic",
    "+warn_export_vars",
    "+warn_shadow_vars",
    "+warn_obsolete_guard",
]

def standard_erlang_tools(index):
    erlang_build(
        name = "otp",
        sources = native.glob(
            ["**/*"],
            exclude = ["BUILD.bazel", "WORKSPACE.bazel"],
        ),
        extra_env = select({
            "@bazel_tools//src/conditions:darwin": {
                "CC": "clang",
            },
            "//conditions:default": {},
        }),
        extra_configure_opts = select({
            "@bazel_tools//src/conditions:darwin": [
                "--enable-darwin-64bit",
                # "--with-ssl=$(brew --prefix openssl@1.1)",
            ],
            "//conditions:default": [],
        }),
        index = index,
    )

    erlang_tool(
        name = "erl",
        otp = ":otp",
        path = "bin/erl",
        visibility = ["//visibility:public"],
    )

    erlang_tool(
        name = "erlc",
        otp = ":otp",
        path = "bin/erlc",
        visibility = ["//visibility:public"],
    )

    erlang_tool(
        name = "escript",
        otp = ":otp",
        path = "bin/escript",
        visibility = ["//visibility:public"],
    )

    erlang_installation(
        name = "erlang_installation_minimal",
        otp = ":otp",
        erl = ":erl",
        erlc = ":erlc",
        escript = ":escript",
    )

    erlang_bytecode(
        name = "app_file_tool_beam",
        erlang_installation = ":erlang_installation_minimal",
        erlc_opts = ERLC_OPTS,
        srcs = [
            Label("//tools/app_file_tool:src/app_file_tool.erl"),
        ],
    )

    escript_flat(
        name = "app_file_tool",
        erlang_installation = ":erlang_installation_minimal",
        beam = ":app_file_tool_beam",
    )

    erlang_bytecode(
        name = "compile_first_beam",
        erlang_installation = ":erlang_installation_minimal",
        erlc_opts = ERLC_OPTS,
        srcs = [
            Label("//tools/compile_first:src/compile_first.erl"),
        ],
    )

    escript_flat(
        name = "compile_first",
        erlang_installation = ":erlang_installation_minimal",
        beam = ":compile_first_beam",
    )

    erlang_bytecode(
        name = "shard_suite_beam",
        dest = "alt",
        erlang_installation = ":erlang_installation_minimal",
        erlc_opts = ERLC_OPTS,
        srcs = [
            Label("//tools/shard_suite:src/shard_suite.erl"),
        ],
    )

    escript_flat(
        name = "shard_suite",
        erlang_installation = ":erlang_installation_minimal",
        beam = ":shard_suite_beam",
    )

    erlang_installation(
        name = "erlang_installation_compilation",
        otp = ":otp",
        erl = ":erl",
        erlc = ":erlc",
        escript = ":escript",
        app_file_tool = ":app_file_tool",
        compile_first = ":compile_first",
        visibility = ["//:__subpackages__"],
    )

    erlang_installation(
        name = "erlang_installation",
        otp = ":otp",
        erl = ":erl",
        erlc = ":erlc",
        escript = ":escript",
        app_file_tool = ":app_file_tool",
        compile_first = ":compile_first",
        shard_suite = ":shard_suite",
        xrefr = "//bazel/xref_runner:xrefr",
        visibility = ["//visibility:public"],
    )

def installation_suffix(erlang_installation):
    wn = Label(erlang_installation).workspace_name
    return wn.removeprefix("rules_erlang").removeprefix(".erlang_package.")
