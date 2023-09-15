load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
)

def _impl(ctx):
    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    script = """set -euo pipefail

{maybe_install_erlang}

exec env PATH="{erlang_home}/bin:$PATH" {worker} $@
""".format(
        maybe_install_erlang = maybe_install_erlang(ctx, True),
        erlang_home = erlang_home,
        worker = ctx.file.worker.path,
    )

    ctx.actions.write(
        output = ctx.outputs.executable,
        content = script,
        is_executable = True,
    )

    runfiles = runfiles.merge(
        ctx.runfiles(files = ctx.files.worker),
    )

    return [
        DefaultInfo(
            runfiles = runfiles,
            executable = ctx.outputs.executable,
        ),
    ]

worker_wrapper = rule(
    implementation = _impl,
    attrs = {
        "worker": attr.label(
            mandatory = True,
            allow_single_file = True,
        ),
    },
    toolchains = ["//tools:toolchain_type"],
    executable = True,
)
