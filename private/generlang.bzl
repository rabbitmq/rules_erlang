load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_symlink_erlang",
)

def _impl(ctx):
    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    outs = [
        ctx.actions.declare_file(out)
        for out in ctx.attr.outs
    ]

    inputs = depset(
        direct = ctx.files.srcs,
        transitive = [runfiles.files],
    )

    cmd = ctx.attr.cmd.replace("$@", outs[0].path)

    script = """set -euo pipefail

{maybe_symlink_erlang}

export PATH="{erlang_home}"/bin:${{PATH}}

{cmd}
""".format(
        maybe_symlink_erlang = maybe_symlink_erlang(ctx),
        erlang_home = erlang_home,
        cmd = cmd,
    )

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = outs,
        command = script,
    )

    return [
        DefaultInfo(files = depset(outs)),
    ]

generlang = rule(
    implementation = _impl,
    attrs = {
        "srcs": attr.label_list(
            allow_files = True,
        ),
        "outs": attr.string_list(),
        "cmd": attr.string(
            mandatory = True,
        ),
    },
    toolchains = ["//tools:toolchain_type"],
)
