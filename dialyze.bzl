load(
    ":erlang_home.bzl",
    "ErlangHomeProvider",
    "ErlangVersionProvider",
)
load(":erlang_app_info.bzl", "ErlangAppInfo")
load(
    ":util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
)
load(":ct.bzl", "code_paths")

DEFAULT_PLT_APPS = ["erts", "kernel", "stdlib"]

def _plt_impl(ctx):
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
    implementation = _plt_impl,
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

def _dialyze_impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    lib_info = ctx.attr.target[ErlangAppInfo]

    if lib_info.erlang_version != erlang_version:
        fail("Erlang version mismatch ({} != {})".format(
            lib_info.erlang_version,
            ctx.attr._erlang_version,
        ))

    apps_args = ""
    if len(ctx.attr.plt_apps) > 0:
        apps_args = "--apps " + " ".join(ctx.attr.plt_apps)

    if ctx.attr.plt == None:
        plt_args = "--build_plt"
    else:
        plt_args = "--plt " + ctx.file.plt.short_path

    dirs = code_paths(ctx, ctx.attr.target)
    for dep in lib_info.deps:
        dirs.extend(code_paths(ctx, dep))

    script = """set -euo pipefail

export HOME=${{TEST_TMPDIR}}

{begins_with_fun}
V=$({erlang_home}/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

set -x
{erlang_home}/bin/dialyzer {apps_args} {plt_args}\\
    -c {dirs} {opts} || test $? -eq 2
""".format(
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
        erlang_version = erlang_version,
        apps_args = apps_args,
        plt_args = plt_args,
        name = ctx.label.name,
        dirs = " ".join(dirs),
        opts = " ".join(ctx.attr.dialyzer_opts),
    )

    ctx.actions.write(
        output = ctx.outputs.executable,
        content = script,
    )

    runfiles = ctx.runfiles([ctx.file.plt] if ctx.file.plt != None else [])
    runfiles = runfiles.merge(ctx.attr.target[DefaultInfo].default_runfiles)
    return [DefaultInfo(runfiles = runfiles)]

dialyze_test = rule(
    implementation = _dialyze_impl,
    attrs = {
        "_erlang_home": attr.label(
            default = Label("//:erlang_home"),
        ),
        "_erlang_version": attr.label(
            default = Label("//:erlang_version"),
        ),
        "is_windows": attr.bool(mandatory = True),
        "plt": attr.label(
            allow_single_file = [".plt"],
        ),
        "target": attr.label(
            providers = [ErlangAppInfo],
            mandatory = True,
        ),
        "plt_apps": attr.string_list(),
        "dialyzer_opts": attr.string_list(
            default = [
                "-Werror_handling",
                "-Wrace_conditions",
                "-Wunmatched_returns",
            ],
        ),
    },
    test = True,
)

def dialyze(**kwargs):
    dialyze_test(
        name = "dialyze",
        target = ":erlang_app",
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
