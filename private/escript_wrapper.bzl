load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
    "version_file",
)

def _impl(ctx):
    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    script = """#!/usr/bin/env bash
set -euo pipefail

{maybe_install_erlang}

exec \\
    env ERLANG_HOME="{erlang_home}" \\
        VERSION_FILE="{version_file}" \\
    "{erlang_home}"/bin/escript "{escript}" $@
""".format(
        maybe_install_erlang = maybe_install_erlang(ctx),
        erlang_home = erlang_home,
        version_file = version_file(ctx).path,
        escript = ctx.file.escript.path,
    )

    ctx.actions.write(
        output = ctx.outputs.out,
        content = script,
        is_executable = True,
    )

    runfiles = runfiles.merge(
        ctx.runfiles(files = ctx.files.escript),
    )

    return [
        DefaultInfo(
            runfiles = runfiles,
            executable = ctx.outputs.out,
        ),
    ]

escript_wrapper = rule(
    implementation = _impl,
    attrs = {
        "escript": attr.label(
            mandatory = True,
            allow_single_file = True,
        ),
        "out": attr.output(
            mandatory = True,
        ),
    },
    toolchains = ["//tools:toolchain_type"],
    executable = True,
)
