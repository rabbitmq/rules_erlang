load("//:erlang_app_info.bzl", "ErlangAppInfo")
load(
    "//:util.bzl",
    "path_join",
    "windows_path",
)
load(":util.bzl", "erl_libs_contents")
load(
    "//tools:erlang_installation.bzl",
    "ErlangInstallationInfo",
    "erlang_dirs",
    "maybe_symlink_erlang",
)

def _to_atom_list(l):
    return "[" + ",".join(["'{}'".format(i) for i in l]) + "]"

def _impl(ctx):
    erl_libs_dir = ctx.label.name + "_deps"

    erl_libs_files = erl_libs_contents(ctx, dir = erl_libs_dir)

    package = ctx.label.package

    erl_libs_path = path_join(package, erl_libs_dir)

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    if not ctx.attr.is_windows:
        test_env_commands = []
        for k, v in ctx.attr.test_env.items():
            test_env_commands.append("export {}=\"{}\"".format(k, v))

        output = ctx.actions.declare_file(ctx.label.name)
        script = """set -euo pipefail

{maybe_symlink_erlang}

export HOME=${{TEST_TMPDIR}}
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
            maybe_symlink_erlang = maybe_symlink_erlang(ctx, short_path = True),
            erlang_home = erlang_home,
            erl_libs_path = erl_libs_path,
            package = package,
            eunit_mods_term = _to_atom_list(ctx.attr.eunit_mods),
            test_env = "\n".join(test_env_commands),
        )
    else:
        test_env_commands = []
        for k, v in ctx.attr.test_env.items():
            test_env_commands.append("set {}={}".format(k, v))

        output = ctx.actions.declare_file(ctx.label.name + ".bat")
        script = """@echo off
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
            erlang_home = windows_path(erlang_home),
            erl_libs_path = erl_libs_path,
            eunit_mods_term = _to_atom_list(ctx.attr.eunit_mods),
            test_env = "\n".join(test_env_commands),
        )

    ctx.actions.write(
        output = output,
        content = script,
    )

    runfiles = runfiles.merge_all(
        [ctx.runfiles(
            files = ctx.files.compiled_suites + ctx.files.data,
            transitive_files = [erl_libs_files],
        )] + [
            tool[DefaultInfo].default_runfiles
            for tool in ctx.attr.tools
        ],
    )

    return [DefaultInfo(
        runfiles = runfiles,
        executable = output,
    )]

eunit_test = rule(
    implementation = _impl,
    attrs = {
        "erlang_installation": attr.label(
            mandatory = True,
            providers = [ErlangInstallationInfo],
        ),
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
