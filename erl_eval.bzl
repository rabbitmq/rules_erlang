load(":erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load(":util.bzl", "path_join")

def _impl(ctx):
    outs = [ctx.actions.declare_file(f) for f in ctx.attr.outs]

    script = """set -euo pipefail
export SRCS="{srcs}"
export OUTS="{outs}"

# TODO LRB
# export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/mnt/c/tools/elixir-1.13.1/bin:/mnt/c/tools/erl-24.2/bin:/mnt/c/windows/system32:/mnt/c/windows

export PATH=/mnt/c/tools/elixir-1.13.1/bin:/mnt/c/tools/erl-24.2/bin:/mnt/c/windows/system32:/mnt/c/windows
"{erlang_home}/bin/erl.exe" -noshell -eval "$1"
""".format(
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path.replace(" ", "\\ "),
        erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
        srcs = ctx.configuration.host_path_separator.join([src.path for src in ctx.files.srcs]),
        outs = ctx.configuration.host_path_separator.join([out.path for out in outs]),
    )

    ctx.actions.run_shell(
        inputs = ctx.files.srcs,
        outputs = outs,
        command = script,
        arguments = [ctx.attr.expression],
    )

    return [
        DefaultInfo(files = depset(outs)),
    ]

erl_eval = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = ":erlang_home"),
        "_erlang_version": attr.label(default = ":erlang_version"),
        "srcs": attr.label_list(allow_files = True),
        "outs": attr.string_list(),
        "expression": attr.string(
            mandatory = True,
        ),
    },
)
