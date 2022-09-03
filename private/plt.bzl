load("//:erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load(":erlang_bytecode.bzl", "unique_dirnames")
load(":transitions.bzl", "beam_transition")
load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
)

DEFAULT_PLT_APPS = ["erts", "kernel", "stdlib"]

def _impl(ctx):
    logfile = ctx.actions.declare_file(ctx.outputs.plt.basename + ".log")
    home_dir = ctx.actions.declare_directory(ctx.label.name + "_home")

    apps_args = ""
    if len(ctx.attr.apps) > 0:
        apps_args = "--apps " + " ".join(ctx.attr.apps)

    if ctx.file.plt == None:
        source_plt_arg = "--build_plt"
    else:
        source_plt_arg = "--plt " + ctx.file.plt.path + " --no_check_plt --add_to_plt"

    files = []
    for dep in flat_deps(ctx.attr.deps):
        lib_info = dep[ErlangAppInfo]
        files.extend(lib_info.beam)

    dirs = unique_dirnames(files)
    dirs_args = "".join([
        "\n    {} \\".format(d)
        for d in dirs
    ])

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    script = """set -euo pipefail

{maybe_install_erlang}

# without HOME being set, dialyzer will error regarding a default plt
export HOME={home}

set +e
set -x
"{erlang_home}"/bin/dialyzer {apps_args} \\
    {source_plt_arg} \\{dirs_args}
    --output_plt {output} > {logfile}
R=$?
set +x
set -e
if [ ! $R -eq 0 ]; then
    cat {logfile}
fi
exit $R
""".format(
        maybe_install_erlang = maybe_install_erlang(ctx),
        erlang_home = erlang_home,
        home = home_dir.path,
        apps_args = apps_args,
        source_plt_arg = source_plt_arg,
        dirs_args = dirs_args,
        output = ctx.outputs.plt.path,
        logfile = logfile.path,
    )

    inputs = depset(
        direct = ctx.files.plt + files,
        transitive = [runfiles.files],
    )

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = [ctx.outputs.plt, logfile, home_dir],
        command = script,
        mnemonic = "DIALYZER",
    )

plt = rule(
    implementation = _impl,
    attrs = {
        "plt": attr.label(
            allow_single_file = [".plt"],
            cfg = beam_transition,
        ),
        "apps": attr.string_list(
            default = DEFAULT_PLT_APPS,
        ),
        "deps": attr.label_list(
            providers = [ErlangAppInfo],
        ),
        # This attribute is required to use starlark transitions. It allows
        # allowlisting usage of this rule. For more information, see
        # https://docs.bazel.build/versions/master/skylark/config.html#user-defined-transitions
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist",
        ),
    },
    outputs = {
        "plt": ".%{name}.plt",
    },
    toolchains = ["//tools:toolchain_type"],
)
