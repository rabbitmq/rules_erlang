load("//:erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load(":erlang_bytecode.bzl", "unique_dirnames")
load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_symlink_erlang",
)

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

    (erlang_home, erlang_release_dir, runfiles) = erlang_dirs(ctx)

    script = """set -euo pipefail

{maybe_symlink_erlang}

export HOME=$PWD

set -x
"{erlang_home}"/bin/dialyzer {apps_args} \\
    {source_plt_arg} \\{dirs}
    --output_plt {output}
""".format(
        maybe_symlink_erlang = maybe_symlink_erlang(ctx),
        erlang_home = erlang_home,
        apps_args = apps_args,
        source_plt_arg = source_plt_arg,
        dirs = "".join(["\n    {} \\".format(d) for d in dirs]),
        output = ctx.outputs.plt.path,
    )

    inputs = depset(
        direct = ctx.files.plt + files,
        transitive = [runfiles.files],
    )

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = [ctx.outputs.plt],
        command = script,
        mnemonic = "DIALYZER",
    )

plt = rule(
    implementation = _impl,
    attrs = {
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
    toolchains = ["//tools:toolchain_type"],
)
