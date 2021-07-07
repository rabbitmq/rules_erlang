load(":erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load(
    ":bazel_erlang_lib.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
    "path_join",
)

def _impl(ctx):
    outs = [ctx.actions.declare_file(f) for f in ctx.attr.outs]

    script = """set -euo pipefail

{begins_with_fun}
V=$({erlang_home}/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

export SRCS="{srcs}"
export OUTS="{outs}"

{erlang_home}/bin/erl \\
    -noshell \\
    -eval "$1"
""".format(
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
        erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
        srcs = " ".join([src.path for src in ctx.files.srcs]),
        outs = " ".join([out.path for out in outs]),
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
