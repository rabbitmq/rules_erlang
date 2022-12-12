load("//:erlang_app_info.bzl", "ErlangAppInfo")
load(
    "//:util.bzl",
    "path_join",
    "windows_path",
)
load(":util.bzl", "erl_libs_contents")
load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
)

def short_dirname(f):
    if f.is_directory:
        return f.short_path
    else:
        return f.short_path.rpartition("/")[0]

def invert_package(package):
    if package == "":
        return package
    parts = package.split("/")
    return "/".join([".." for p in parts])

def package_relative_dirnames(package, files):
    dirs = []
    for f in files:
        sd = short_dirname(f)
        if sd.startswith(package + "/"):
            rel = sd.removeprefix(package + "/")
        else:
            rel = path_join(invert_package(package), sd)
        if rel not in dirs:
            dirs.append(rel)
    return dirs

def _to_atom_list(l):
    return "[" + ",".join(["'{}'".format(i) for i in l]) + "]"

def _impl(ctx):
    erl_libs_dir = ctx.label.name + "_deps"

    erl_libs_files = erl_libs_contents(ctx, dir = erl_libs_dir)

    package = ctx.label.package

    erl_libs_path = path_join(package, erl_libs_dir)

    pa_args = []
    for dir in package_relative_dirnames(package, ctx.files.compiled_suites):
        pa_args.extend(["-pa", dir])

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    eunit_opts_term = "[" + ",".join(ctx.attr.eunit_opts) + "]"

    if not ctx.attr.is_windows:
        test_env_commands = []
        for k, v in ctx.attr.test_env.items():
            test_env_commands.append("export {}=\"{}\"".format(k, v))

        output = ctx.actions.declare_file(ctx.label.name)
        script = """set -euo pipefail

{maybe_install_erlang}

export HOME=${{TEST_TMPDIR}}
export ERL_LIBS=$TEST_SRCDIR/$TEST_WORKSPACE/{erl_libs_path}

{test_env}

if [ -n "{package}" ]; then
    cd {package}
fi

set -x
"{erlang_home}"/bin/erl +A1 -noinput -boot no_dot_erlang \\
    {pa_args} \\
    -eval "case eunit:test({eunit_mods_term},{eunit_opts_term}) of ok -> ok; error -> halt(2) end, halt()"
""".format(
            maybe_install_erlang = maybe_install_erlang(ctx, short_path = True),
            erlang_home = erlang_home,
            erl_libs_path = erl_libs_path,
            package = package,
            pa_args = " ".join(pa_args),
            eunit_mods_term = _to_atom_list(ctx.attr.eunit_mods),
            eunit_opts_term = eunit_opts_term,
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
    {pa_args} ^
    -eval "case eunit:test({eunit_mods_term},{eunit_opts_term}) of ok -> ok; error -> halt(2) end, halt()" || exit /b 1
""".format(
            package = package,
            erlang_home = windows_path(erlang_home),
            erl_libs_path = erl_libs_path,
            pa_args = " ".join(pa_args),
            eunit_mods_term = _to_atom_list(ctx.attr.eunit_mods),
            eunit_opts_term = eunit_opts_term,
            test_env = "\n".join(test_env_commands),
        )

    ctx.actions.write(
        output = output,
        content = script,
    )

    runfiles = runfiles.merge_all(
        [ctx.runfiles(
            files = ctx.files.compiled_suites + ctx.files.data,
            transitive_files = depset(erl_libs_files),
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
        "is_windows": attr.bool(mandatory = True),
        "compiled_suites": attr.label_list(
            allow_files = [".beam"],
            mandatory = True,
        ),
        "eunit_mods": attr.string_list(mandatory = True),
        "eunit_opts": attr.string_list(),
        "data": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "tools": attr.label_list(),
        "test_env": attr.string_dict(),
    },
    toolchains = ["//tools:toolchain_type"],
    test = True,
)
