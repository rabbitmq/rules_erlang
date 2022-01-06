load(
    ":erlang_home.bzl",
    "ErlangHomeProvider",
    "ErlangVersionProvider",
)
load(
    ":erlang_app_info.bzl",
    "ErlangAppInfo",
    "flat_deps",
)
load(":ct.bzl", "code_paths")

def _impl(ctx):
    paths = []
    for dep in flat_deps(ctx.attr.deps):
        paths.extend(code_paths(ctx, dep))

    script = """
set -euo pipefail
set -x
"{erlang_home}/bin/erl.exe" {pa_args} {extra_erl_args}
""".format(
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
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "extra_erl_args": attr.string_list(),
    },
    executable = True,
)
