load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
)
load("//:erlang_app_info.bzl", "ErlangAppInfo")
load("//:util.bzl", "windows_path")
load(":transitions.bzl", "beam_transition")
load(":ct.bzl", "code_paths")

def _impl(ctx):
    apps_args = ""
    if len(ctx.attr.plt_apps) > 0:
        apps_args = "--apps " + " ".join(ctx.attr.plt_apps)

    if ctx.attr.plt == None:
        plt_args = "--build_plt"
    elif not ctx.attr.is_windows:
        plt_args = "--plt " + ctx.file.plt.short_path + " --no_check_plt"
    else:
        plt_args = "--plt " + windows_path(ctx.file.plt.short_path) + " --no_check_plt"

    dirs = code_paths(ctx.attr.target)

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    if not ctx.attr.is_windows:
        output = ctx.actions.declare_file(ctx.label.name)
        script = """set -euo pipefail

{maybe_install_erlang}

export HOME=${{TEST_TMPDIR}}

set -x
"{erlang_home}"/bin/dialyzer {apps_args} \\
    {plt_args} \\
    -r {dirs} {opts}{check_warnings}
""".format(
            maybe_install_erlang = maybe_install_erlang(ctx, short_path = True),
            erlang_home = erlang_home,
            apps_args = apps_args,
            plt_args = plt_args,
            dirs = " ".join(dirs),
            opts = " ".join(ctx.attr.dialyzer_opts),
            check_warnings = " || test $? -eq 2" if not ctx.attr.warnings_as_errors else "",
        )
    else:
        output = ctx.actions.declare_file(ctx.label.name + ".bat")
        script = """
"{erlang_home}\\bin\\dialyzer" {apps_args} ^
    {plt_args} ^
    -r {dirs} {opts}
if %ERRORLEVEL% EQU 0 EXIT /B 0
{check_warnings}
EXIT /B 1
""".format(
            erlang_home = windows_path(erlang_home),
            apps_args = apps_args,
            plt_args = plt_args,
            dirs = " ".join(dirs),
            opts = " ".join(ctx.attr.dialyzer_opts),
            check_warnings = "if %ERRORLEVEL% EQU 2 EXIT /B 0" if not ctx.attr.warnings_as_errors else "",
        )

    ctx.actions.write(
        output = output,
        content = script,
    )

    runfiles = runfiles.merge_all([
        ctx.runfiles(ctx.files.plt),
        ctx.attr.target[DefaultInfo].default_runfiles,
    ])
    return [DefaultInfo(
        runfiles = runfiles,
        executable = output,
    )]

dialyze_test = rule(
    implementation = _impl,
    attrs = {
        "is_windows": attr.bool(mandatory = True),
        "plt": attr.label(
            allow_single_file = [".plt"],
            cfg = beam_transition,
        ),
        "target": attr.label(
            providers = [ErlangAppInfo],
            mandatory = True,
        ),
        "plt_apps": attr.string_list(),
        "dialyzer_opts": attr.string_list(
            default = [
                "-Werror_handling",
                "-Wunmatched_returns",
            ],
        ),
        "warnings_as_errors": attr.bool(default = True),
        # This attribute is required to use starlark transitions. It allows
        # allowlisting usage of this rule. For more information, see
        # https://docs.bazel.build/versions/master/skylark/config.html#user-defined-transitions
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist",
        ),
    },
    toolchains = ["//tools:toolchain_type"],
    test = True,
)
