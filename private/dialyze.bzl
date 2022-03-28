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

def _impl(ctx):
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
    elif not ctx.attr.is_windows:
        plt_args = "--plt " + ctx.file.plt.short_path + " --no_check_plt"
    else:
        plt_args = "--plt " + windows_path(ctx.file.plt.short_path) + " --no_check_plt"

    dirs = code_paths(ctx.attr.target)

    if not ctx.attr.is_windows:
        output = ctx.actions.declare_file(ctx.label.name)
        script = """set -euo pipefail

export HOME=${{TEST_TMPDIR}}

{begins_with_fun}
V=$({erlang_home}/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

set -x
{erlang_home}/bin/dialyzer {apps_args} \\
    {plt_args} \\
    -r {dirs} {opts}{check_warnings}
""".format(
            begins_with_fun = BEGINS_WITH_FUN,
            query_erlang_version = QUERY_ERL_VERSION,
            erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
            erlang_version = erlang_version,
            apps_args = apps_args,
            plt_args = plt_args,
            dirs = " ".join(dirs),
            opts = " ".join(ctx.attr.dialyzer_opts),
            check_warnings = " || test $? -eq 2" if not ctx.attr.warnings_as_errors else "",
        )
    else:
        output = ctx.actions.declare_file(ctx.label.name + ".bat")
        script = """@echo off
echo Erlang Version: {erlang_version}

echo on
"{erlang_home}\\bin\\dialyzer" {apps_args} ^
    {plt_args} ^
    -r {dirs} {opts}
if %ERRORLEVEL% EQU 0 EXIT /B 0
{check_warnings}
EXIT /B 1
""".format(
            erlang_home = windows_path(ctx.attr._erlang_home[ErlangHomeProvider].path),
            erlang_version = erlang_version,
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

    runfiles = ctx.runfiles([ctx.file.plt] if ctx.file.plt != None else [])
    runfiles = runfiles.merge(ctx.attr.target[DefaultInfo].default_runfiles)
    return [DefaultInfo(
        runfiles = runfiles,
        executable = output,
    )]

dialyze_test = rule(
    implementation = _impl,
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
                "-Wunmatched_returns",
            ],
        ),
        "warnings_as_errors": attr.bool(default = True),
    },
    test = True,
)
