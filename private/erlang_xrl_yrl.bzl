load("//:util.bzl", "path_join")
load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
)
load(":erlang_app_sources.bzl", "ErlangGeneratedCodeInfo")

def _impl(ctx):
    package_dir = path_join(
        ctx.label.workspace_root,
        ctx.label.package,
    )

    srcs = ctx.actions.args()
    srcs.add_all(ctx.files.srcs)

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    src_outputs = []
    for src in ctx.files.srcs:
        fname = src.basename.removesuffix(".xrl").removesuffix(".yrl")
        src_outputs.append(ctx.actions.declare_file(path_join("src", fname + ".erl")))

    script = """#!/usr/bin/env bash

set -euo pipefail

{maybe_install_erlang}

mkdir -p {src_out}

"{erlang_home}"/bin/erlc \\
    -o "{src_out}" \\
    "$@"
    """.format(
        maybe_install_erlang = maybe_install_erlang(ctx),
        erlang_home = erlang_home,
        src_out = src_outputs[0].dirname,
    )

    inputs = depset(
        direct = ctx.files.srcs,
        transitive = [runfiles.files],
    )

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = src_outputs,
        command = script,
        arguments = [srcs],
        mnemonic = "ERLXRLYRL",
    )

    return [
        ErlangGeneratedCodeInfo(
            srcs = depset(src_outputs),
            includes = depset([]),
        ),
        DefaultInfo(files = depset(src_outputs)),
    ]

erlang_xrl_yrl = rule(
    implementation = _impl,
    attrs = {
        "srcs": attr.label_list(
            mandatory = True,
            doc = "The xrl or yrl files to compile",
            allow_files = [".xrl", ".yrl"],
        ),
    },
    toolchains = ["//tools:toolchain_type"],
    provides = [ErlangGeneratedCodeInfo],
)
