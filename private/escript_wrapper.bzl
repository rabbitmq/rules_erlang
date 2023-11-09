load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
)

def _impl(ctx):
    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    script = """set -euxo pipefail
{maybe_install_erlang}
exec "{erlang_home}"/bin/escript {escript} $@
""".format(
        maybe_install_erlang = maybe_install_erlang(ctx, True),
        erlang_home = erlang_home,
        escript = ctx.file.escript.path,
    )

    ctx.actions.write(
        output = ctx.outputs.executable,
        content = script,
        is_executable = True,
    )

    runfiles = runfiles.merge(
        ctx.runfiles(files = ctx.files.escript),
    )

    return [
        DefaultInfo(
            runfiles = runfiles,
            executable = ctx.outputs.executable,
        ),
    ]

escript_wrapper = rule(
    implementation = _impl,
    attrs = {
        "escript": attr.label(
            mandatory = True,
            allow_single_file = True,
        ),
    },
    toolchains = ["//tools:toolchain_type"],
    executable = True,
)
