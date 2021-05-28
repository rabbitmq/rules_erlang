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
    "path_join",
)
load(":ct.bzl", "code_paths")

DEFAULT_PLT_APPS = ["erts", "kernel", "stdlib"]

def _to_erlang_string_list(strings):
    return "[" + ",".join(["\"{}\"".format(s) for s in strings]) + "]"

def _to_erlang_atom_list(strings):
    return "[" + ",".join(strings) + "]"

def _impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    lib_info = ctx.attr.target[ErlangLibInfo]

    if lib_info.erlang_version != erlang_version:
        fail("Erlang version mismatch ({} != {})".format(
            lib_info.erlang_version,
            ctx.attr._erlang_version,
        ))

    apps_args = ""
    if len(ctx.attr.plt_apps) > 0:
        apps_args = "--apps " + " ".join(ctx.attr.plt_apps)

    dirs = code_paths(ctx.attr.target)
    for dep in lib_info.deps:
        dirs.extend(code_paths(dep))

    script = """set -euxo pipefail

export HOME=${{TEST_TMPDIR}}

{begins_with_fun}
V=$({erlang_home}/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

{erlang_home}/bin/dialyzer {apps_args}\\
    --build_plt \\
    --output_plt ${{TEST_UNDECLARED_OUTPUTS_DIR}}/{name}.plt \\
    -c {dirs}
""".format(
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
        erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
        apps_args = apps_args,
        name = ctx.label.name,
        dirs = " ".join(dirs),
    )

    ctx.actions.write(
        output = ctx.outputs.executable,
        content = script,
    )

    runfiles = ctx.attr.target[DefaultInfo].default_runfiles
    return [DefaultInfo(runfiles = runfiles)]

dialyze_test = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(
            default = Label("//:erlang_home"),
        ),
        "_erlang_version": attr.label(
            default = Label("//:erlang_version"),
        ),
        "target": attr.label(
            providers = [ErlangLibInfo],
            mandatory = True,
        ),
        "plt_apps": attr.string_list(
            default = DEFAULT_PLT_APPS,
        ),
    },
    test = True,
)

def dialyze(**kwargs):
    dialyze_test(
        name = "dialyze",
        target = ":bazel_erlang_lib",
        **kwargs
    )
