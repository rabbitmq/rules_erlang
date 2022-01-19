load(
    "//:erlang_home.bzl",
    "ErlangHomeProvider",
    "ErlangVersionProvider",
)
load("//:erlang_app_info.bzl", "ErlangAppInfo")
load(
    "//:util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
    "windows_path",
)
load(":ct.bzl", "code_paths")

DEFAULT_PLT_APPS = ["erts", "kernel", "stdlib"]

def _impl(ctx):
    apps_args = ""
    if len(ctx.attr.apps) > 0:
        apps_args = "--apps " + " ".join(ctx.attr.apps)

    if ctx.attr.plt == None:
        source_plt_arg = "--build_plt"
    else:
        source_plt_arg = "--plt " + ctx.file.plt.path + " --add_to_plt"

    script = """set -euo pipefail

export HOME=$PWD

{begins_with_fun}
V=$("{erlang_home}"/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

set -x
"{erlang_home}"/bin/dialyzer {apps_args} {source_plt_arg}\\
    --output_plt {output}
""".format(
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
        erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
        apps_args = apps_args,
        source_plt_arg = source_plt_arg,
        output = ctx.outputs.plt.path,
    )

    ctx.actions.run_shell(
        inputs = [ctx.file.plt] if ctx.file.plt != None else [],
        outputs = [ctx.outputs.plt],
        command = script,
        mnemonic = "DIALYZER",
    )

plt = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(
            default = Label("//:erlang_home"),
        ),
        "_erlang_version": attr.label(
            default = Label("//:erlang_version"),
        ),
        "plt": attr.label(
            allow_single_file = [".plt"],
        ),
        "apps": attr.string_list(
            default = DEFAULT_PLT_APPS,
        ),
    },
    outputs = {
        "plt": ".%{name}.plt",
    },
)
