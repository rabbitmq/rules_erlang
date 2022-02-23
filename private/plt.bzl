load(
    "//:erlang_home.bzl",
    "ErlangHomeProvider",
    "ErlangVersionProvider",
)
load("//:erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load(
    "//:util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
    "windows_path",
)
load(":erlc.bzl", "unique_dirnames")
load(":ct.bzl", "code_paths")

DEFAULT_PLT_APPS = ["erts", "kernel", "stdlib"]

def _impl(ctx):
    apps_args = ""
    if len(ctx.attr.apps) > 0:
        apps_args = "--apps " + " ".join(ctx.attr.apps)

    if ctx.attr.plt == None:
        source_plt_arg = "--build_plt"
    else:
        source_plt_arg = "--plt " + ctx.file.plt.path + " --no_check_plt --add_to_plt"

    files = []
    for dep in flat_deps(ctx.attr.deps):
        lib_info = dep[ErlangAppInfo]
        files.extend(lib_info.beam)

    dirs = unique_dirnames(files)

    script = """set -euo pipefail

export HOME=$PWD

{begins_with_fun}
V=$("{erlang_home}"/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

set -x
"{erlang_home}"/bin/dialyzer {apps_args} \\
    {source_plt_arg} \\{dirs}
    --output_plt {output}
""".format(
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
        erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
        apps_args = apps_args,
        source_plt_arg = source_plt_arg,
        dirs = "".join(["\n    {} \\".format(d) for d in dirs]),
        output = ctx.outputs.plt.path,
    )

    inputs = []
    if ctx.file.plt != None:
        inputs.append(ctx.file.plt)
    inputs.extend(files)

    ctx.actions.run_shell(
        inputs = inputs,
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
        "deps": attr.label_list(
            providers = [ErlangAppInfo],
        ),
    },
    outputs = {
        "plt": ".%{name}.plt",
    },
)
