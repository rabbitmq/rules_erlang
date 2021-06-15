load(
    ":erlang_home.bzl",
    "ErlangHomeProvider",
    "ErlangVersionProvider",
)
load(
    ":bazel_erlang_lib.bzl",
    "BEGINS_WITH_FUN",
    "ErlangLibInfo",
    "QUERY_ERL_VERSION",
    "flat_deps",
    "path_join",
)
load(":ct.bzl", "code_paths")

def _impl(ctx):
    paths = []
    for dep in flat_deps(ctx.attr.deps):
        paths.extend(code_paths(dep))

    script = """
set -euo pipefail

{begins_with_fun}
V=$({erlang_home}/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

set -x
{erlang_home}/bin/erl {pa_args} {extra_erl_args}
""".format(
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
        erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
        pa_args = " ".join(["-pa {}".format(p) for p in paths]),
        extra_erl_args = " ".join(ctx.attr.extra_erl_args),
    )

    ctx.actions.write(
        output = ctx.outputs.executable,
        content = script,
    )

    runfiles = ctx.runfiles([])
    for dep in ctx.attr.deps:
        runfiles = runfiles.merge(dep[DefaultInfo].default_runfiles)

    return [
        DefaultInfo(runfiles = runfiles),
    ]

shell = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(
            default = Label("//:erlang_home"),
        ),
        "_erlang_version": attr.label(
            default = Label("//:erlang_version"),
        ),
        "deps": attr.label_list(providers = [ErlangLibInfo]),
        "extra_erl_args": attr.string_list(),
    },
    executable = True,
)
