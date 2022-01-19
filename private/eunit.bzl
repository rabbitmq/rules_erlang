load("//:erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load("//:erlang_app_info.bzl", "ErlangAppInfo")
load("//:erlang_app.bzl", "DEFAULT_TEST_ERLC_OPTS")
load("//:erlc.bzl", "erlc")
load(
    "//:util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
    "path_join",
    "windows_path",
)
load(
    ":ct.bzl",
    "ERL_LIBS_DIR",
    "erl_libs_contents",
)

def _to_atom_list(l):
    return "[" + ",".join(["'{}'".format(i) for i in l]) + "]"

def _impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    erl_libs_files = erl_libs_contents(ctx)

    package = ctx.label.package

    erl_libs_path = path_join(package, ERL_LIBS_DIR)

    if not ctx.attr.is_windows:
        test_env_commands = []
        for k, v in ctx.attr.test_env.items():
            test_env_commands.append("export {}=\"{}\"".format(k, v))

        output = ctx.actions.declare_file(ctx.label.name)
        script = """set -euo pipefail

export HOME=${{TEST_TMPDIR}}

{begins_with_fun}
V=$("{erlang_home}"/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

export ERL_LIBS=$TEST_SRCDIR/$TEST_WORKSPACE/{erl_libs_path}

{test_env}

if [ -n "{package}" ]; then
    cd {package}
fi

set -x
"{erlang_home}"/bin/erl +A1 -noinput -boot no_dot_erlang \\
    -pa test \\
    -eval "case eunit:test({eunit_mods_term},[]) of ok -> ok; error -> halt(2) end, halt()"
""".format(
            begins_with_fun = BEGINS_WITH_FUN,
            query_erlang_version = QUERY_ERL_VERSION,
            package = package,
            erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
            erlang_version = erlang_version,
            erl_libs_path = erl_libs_path,
            eunit_mods_term = _to_atom_list(ctx.attr.eunit_mods),
            test_env = "\n".join(test_env_commands),
        )
    else:
        test_env_commands = []
        for k, v in ctx.attr.test_env.items():
            test_env_commands.append("set {}={}".format(k, v))

        output = ctx.actions.declare_file(ctx.label.name + ".bat")
        script = """@echo off
echo Erlang Version: {erlang_version}

REM TEST_SRCDIR is provided by bazel but with unix directory separators
set ERL_LIBS=%TEST_SRCDIR%/%TEST_WORKSPACE%/{erl_libs_path}
set ERL_LIBS=%ERL_LIBS:/=\\%

{test_env}

if NOT [{package}] == [] cd {package}

echo on
"{erlang_home}\\bin\\erl" +A1 -noinput -boot no_dot_erlang ^
    -pa test ^
    -eval "case eunit:test({eunit_mods_term},[]) of ok -> ok; error -> halt(2) end, halt()" || exit /b 1
""".format(
            package = package,
            erlang_home = windows_path(ctx.attr._erlang_home[ErlangHomeProvider].path),
            erlang_version = erlang_version,
            erl_libs_path = erl_libs_path,
            eunit_mods_term = _to_atom_list(ctx.attr.eunit_mods),
            test_env = "\n".join(test_env_commands),
        )

    ctx.actions.write(
        output = output,
        content = script,
    )

    runfiles = ctx.runfiles(
        files = ctx.files.compiled_suites + ctx.files.data,
        transitive_files = depset(erl_libs_files),
    )
    for tool in ctx.attr.tools:
        runfiles = runfiles.merge(tool[DefaultInfo].default_runfiles)

    return [DefaultInfo(
        runfiles = runfiles,
        executable = output,
    )]

eunit_test = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
        "is_windows": attr.bool(mandatory = True),
        "compiled_suites": attr.label_list(
            allow_files = [".beam"],
            mandatory = True,
        ),
        "eunit_mods": attr.string_list(mandatory = True),
        "data": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "tools": attr.label_list(),
        "test_env": attr.string_dict(),
    },
    test = True,
)
