load("//:erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load("//:erlang_app.bzl", "DEFAULT_TEST_ERLC_OPTS")
load("//:erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load("//:erlc.bzl", "erlc")
load(
    ":ct.bzl",
    "ERL_LIBS_DIR",
    "erl_libs_contents",
    "short_dirname",
    "sname",
)
load(":erlc.bzl", "beam_file")
load(
    "//:util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
    "path_join",
    "windows_path",
)

def _impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    erl_libs_files = erl_libs_contents(ctx)

    package = ctx.label.package

    erl_libs_path = path_join(package, ERL_LIBS_DIR)

    ct_hooks_args = ""
    if len(ctx.attr.ct_hooks) > 0:
        ct_hooks_args = "-ct_hooks " + " ".join(ctx.attr.ct_hooks)

    if not ctx.attr.is_windows:
        test_env_commands = []
        for k, v in ctx.attr.test_env.items():
            test_env_commands.append("export {}=\"{}\"".format(k, v))

        output = ctx.actions.declare_file(ctx.label.name)
        script = """set -eo pipefail

if [ -n "${{TEST_SHARD_STATUS_FILE+x}}" ]; then
    touch ${{TEST_SHARD_STATUS_FILE}}
fi

export HOME=${{TEST_TMPDIR}}

{begins_with_fun}
V=$("{erlang_home}"/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

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
            begins_with_fun = BEGINS_WITH_FUN,
            query_erlang_version = QUERY_ERL_VERSION,
            package = package,
            erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
            erlang_version = erlang_version,
            erl_libs_path = erl_libs_path,
            shard_suite = ctx.file._shard_suite_escript.short_path,
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
echo Erlang Version: {erlang_version}

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
            erlang_home = windows_path(ctx.attr._erlang_home[ErlangHomeProvider].path),
            erlang_version = erlang_version,
            erl_libs_path = erl_libs_path,
            shard_suite = ctx.file._shard_suite_escript.short_path,
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

    runfiles = ctx.runfiles(
        files = ctx.files.compiled_suites + ctx.files.data + ctx.files._shard_suite_escript,
        transitive_files = depset(erl_libs_files),
    )
    for tool in ctx.attr.tools:
        runfiles = runfiles.merge(tool[DefaultInfo].default_runfiles)

    return [DefaultInfo(
        runfiles = runfiles,
        executable = output,
    )]

ct_sharded_test = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
        "_shard_suite_escript": attr.label(
            default = Label("//shard_suite:escript"),
            allow_single_file = True,
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
