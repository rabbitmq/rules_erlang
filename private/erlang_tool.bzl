load(
    ":erlang_installation.bzl",
    "erlang_dirs",
    "maybe_symlink_erlang",
)

DEFAULT_PATH = "bin/erl"

def _impl(ctx):
    out = ctx.actions.declare_file(ctx.label.name)

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    ctx.actions.write(
        output = out,
        content = """set -euo pipefail

{maybe_symlink_erlang}

exec "{erlang_home}"/{path} $@
""".format(
            maybe_symlink_erlang = maybe_symlink_erlang(ctx, short_path = True),
            erlang_home = erlang_home,
            path = ctx.attr.path,
        ),
    )

    return [
        DefaultInfo(
            executable = out,
            runfiles = runfiles,
        ),
    ]

erlang_tool = rule(
    implementation = _impl,
    attrs = {
        "erlang_installation": attr.label(
            allow_single_file = True,
            mandatory = True,
        ),
        "path": attr.string(default = DEFAULT_PATH),
    },
    executable = True,
)