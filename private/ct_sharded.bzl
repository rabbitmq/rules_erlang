load("//:erlang_app_info.bzl", "ErlangAppInfo")
load(
    ":ct.bzl",
    "short_dirname",
    "sname",
)
load(":util.bzl", "erl_libs_contents")
load(
    "//:util.bzl",
    "path_join",
    "windows_path",
)
load(
    "//tools:erlang_installation.bzl",
    "ErlangInstallationInfo",
    "erlang_dirs",
    "maybe_symlink_erlang",
)

def _impl(ctx):
    erl_libs_dir = ctx.label.name + "_deps"

    erl_libs_files = erl_libs_contents(ctx, dir = erl_libs_dir)

    package = ctx.label.package

    erl_libs_path = path_join(package, erl_libs_dir)

    ct_hooks_args = ""
    if len(ctx.attr.ct_hooks) > 0:
        ct_hooks_args = "-ct_hooks " + " ".join(ctx.attr.ct_hooks)

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    shard_suite = ctx.attr.erlang_installation[ErlangInstallationInfo].shard_suite
    shard_suite_path = shard_suite[DefaultInfo].files_to_run.executable.short_path

    if not ctx.attr.is_windows:
        test_env_commands = []
        for k, v in ctx.attr.test_env.items():
            test_env_commands.append("export {}=\"{}\"".format(k, v))

        output = ctx.actions.declare_file(ctx.label.name)
        script = """set -eo pipefail

{maybe_symlink_erlang}

if [ -n "${{TEST_SHARD_STATUS_FILE+x}}" ]; then
    touch ${{TEST_SHARD_STATUS_FILE}}
fi

export HOME=${{TEST_TMPDIR}}
export ERL_LIBS=$TEST_SRCDIR/$TEST_WORKSPACE/{erl_libs_path}

{test_env}

if [ -n "${{FOCUS+x}}" ]; then
    if [ -n "${{TEST_SHARD_STATUS_FILE+x}}" ]; then
        if [ 0 -eq ${{TEST_SHARD_INDEX}} ]; then
            echo "Using shard index 0 to run FOCUS'ed tests"
            FILTER="-suite {suite_name} ${{FOCUS}}"
        else
            echo "Skipping shard ${{TEST_SHARD_INDEX}} as FOCUS is set"
            exit 0
        fi
    else
        FILTER="-suite {suite_name} ${{FOCUS}}"
    fi
else
    if [ -n "${{TEST_SHARD_STATUS_FILE+x}}" ]; then
        export SHARD_SUITE_CODE_PATHS="$TEST_SRCDIR/$TEST_WORKSPACE/{dir}"
        FILTER=$("{erlang_home}"/bin/escript \\
            $TEST_SRCDIR/$TEST_WORKSPACE/{shard_suite} \\
                -{sharding_method} \\
                {suite_name} \\
                ${{TEST_SHARD_INDEX}} \\
                ${{TEST_TOTAL_SHARDS}})
    else
        FILTER="-suite {suite_name}"
    fi
fi

if [ -n "{package}" ]; then
    cd {package}
fi

set -x
"{erlang_home}"/bin/ct_run \\
    -no_auto_compile \\
    -noinput \\
    ${{FILTER}} \\
    -dir $TEST_SRCDIR/$TEST_WORKSPACE/{dir} \\
    -logdir ${{TEST_UNDECLARED_OUTPUTS_DIR}} \\
    {ct_hooks_args} \\
    -sname {sname}
""".format(
            maybe_symlink_erlang = maybe_symlink_erlang(ctx, short_path = True),
            erlang_home = erlang_home,
            package = package,
            erl_libs_path = erl_libs_path,
            shard_suite = shard_suite_path,
            sharding_method = ctx.attr.sharding_method,
            suite_name = ctx.attr.suite_name,
            dir = short_dirname(ctx.files.compiled_suites[0]),
            ct_hooks_args = ct_hooks_args,
            sname = sname(ctx),
            test_env = "\n".join(test_env_commands),
        )
    else:
        test_env_commands = []
        for k, v in ctx.attr.test_env.items():
            test_env_commands.append("set {}={}".format(k, v))

        output = ctx.actions.declare_file(ctx.label.name + ".bat")
        script = """@echo off
SETLOCAL EnableDelayedExpansion
if defined TEST_SHARD_STATUS_FILE type nul > !TEST_SHARD_STATUS_FILE!

REM TEST_SRCDIR is provided by bazel but with unix directory separators
set dir=%TEST_SRCDIR%/%TEST_WORKSPACE%/{dir}
set dir=%dir:/=\\%

set logdir=%TEST_UNDECLARED_OUTPUTS_DIR%
set logdir=%logdir:/=\\%
subst b: %logdir%

set ERL_LIBS=%TEST_SRCDIR%/%TEST_WORKSPACE%/{erl_libs_path}
set ERL_LIBS=%ERL_LIBS:/=\\%

{test_env}

if NOT defined FOCUS goto :no_focus
if NOT defined TEST_SHARD_STATUS_FILE goto :focus_no_shard
:focus_shard
if [%TEST_SHARD_INDEX%] == [0] set FILTER=-suite {suite_name} %FOCUS% else goto :skip_test
goto :run_test
:focus_no_shard
set FILTER=-suite {suite_name} %FOCUS%
goto :run_test
:no_focus
if NOT defined TEST_SHARD_STATUS_FILE goto :no_focus_no_shard
:no_focus_shard
set SHARD_SUITE_CODE_PATHS=%TEST_SRCDIR%/%TEST_WORKSPACE%/{dir}
set SHARD_SUITE_CODE_PATHS=%SHARD_SUITE_CODE_PATHS:/=\\%
set shard_suite_tool_path=%TEST_SRCDIR%/%TEST_WORKSPACE%/{shard_suite}
set shard_suite_tool_path=%shard_suite_tool_path:/=\\%
"{erlang_home}"\\bin\\escript %shard_suite_tool_path% ^
    -{sharding_method} {suite_name} %TEST_SHARD_INDEX% %TEST_TOTAL_SHARDS% ^
    > shard.tmp
set /p FILTER= < shard.tmp
DEL shard.tmp
goto :run_test
:no_focus_no_shard
set FILTER=-suite {suite_name}

:run_test

if NOT [{package}] == [] cd {package}

echo on
"{erlang_home}\\bin\\ct_run" ^
    -no_auto_compile ^
    -noinput ^
    %FILTER% ^
    -dir %dir% ^
    -logdir b: ^
    {ct_hooks_args} ^
    -sname {sname}
set CT_RUN_ERRORLEVEL=%ERRORLEVEL%
subst b: /d
exit /b %CT_RUN_ERRORLEVEL%
:skip_test
""".format(
            package = package,
            erlang_home = windows_path(erlang_home),
            erl_libs_path = erl_libs_path,
            shard_suite = shard_suite_path,
            sharding_method = ctx.attr.sharding_method,
            suite_name = ctx.attr.suite_name,
            dir = short_dirname(ctx.files.compiled_suites[0]),
            ct_hooks_args = ct_hooks_args,
            sname = sname(ctx),
            test_env = "\n".join(test_env_commands),
        )

    ctx.actions.write(
        output = output,
        content = script,
    )

    runfiles = runfiles.merge_all(
        [
            ctx.runfiles(ctx.files.compiled_suites + ctx.files.data + erl_libs_files),
        ] + [
            shard_suite[DefaultInfo].default_runfiles,
        ] + [
            tool[DefaultInfo].default_runfiles
            for tool in ctx.attr.tools
        ],
    )

    return [DefaultInfo(
        runfiles = runfiles,
        executable = output,
    )]

ct_sharded_test = rule(
    implementation = _impl,
    attrs = {
        "erlang_installation": attr.label(
            mandatory = True,
            providers = [ErlangInstallationInfo],
        ),
        "is_windows": attr.bool(mandatory = True),
        "suite_name": attr.string(mandatory = True),
        "compiled_suites": attr.label_list(
            allow_files = [".beam"],
            mandatory = True,
        ),
        "ct_hooks": attr.string_list(),
        "data": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "tools": attr.label_list(),
        "test_env": attr.string_dict(),
        "sharding_method": attr.string(
            default = "group",
            values = ["group", "case"],
        ),
    },
    test = True,
)
