load("//:erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load("//:erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load(
    "//:util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
    "path_join",
    "windows_path",
)
load(":erlc.bzl", "beam_file")

ERL_LIBS_DIR = "deps"

def sanitize_sname(s):
    return s.replace("@", "-").replace(".", "_")

def short_dirname(f):
    if f.is_directory:
        return f.short_path
    else:
        return f.short_path.rpartition("/")[0]

def _unique_short_dirnames(files):
    dirs = []
    for f in files:
        dirname = short_dirname(f)
        if dirname not in dirs:
            dirs.append(dirname)
    return dirs

def code_paths(dep):
    return [
        path_join(dep.label.workspace_root, d) if dep.label.workspace_root != "" else d
        for d in _unique_short_dirnames(dep[ErlangAppInfo].beam)
    ]

def additional_file_dest_relative_path(dep_label, f):
    if dep_label.workspace_root != "":
        workspace_root = dep_label.workspace_root.replace("external/", "../")
        rel_base = path_join(workspace_root, dep_label.package)
    else:
        rel_base = dep_label.package
    if rel_base != "":
        return f.short_path.replace(rel_base + "/", "")
    else:
        return f.short_path

def erl_libs_contents(ctx, headers = False, dir = ERL_LIBS_DIR):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    erl_libs_files = []
    for dep in flat_deps(ctx.attr.deps):
        lib_info = dep[ErlangAppInfo]
        dep_path = path_join(dir, lib_info.app_name)
        if lib_info.erlang_version != erlang_version:
            fail("Mismatched erlang versions", erlang_version, lib_info.erlang_version)
        if headers:
            for hdr in lib_info.include:
                dest = ctx.actions.declare_file(path_join(dep_path, "include", hdr.basename))
                ctx.actions.symlink(output = dest, target_file = hdr)
                erl_libs_files.append(dest)
        for src in lib_info.beam:
            if src.is_directory:
                if len(lib_info.beam) != 1:
                    fail("ErlangAppInfo.beam must be a collection of files, or a single ebin dir")
                dest = ctx.actions.declare_directory(path_join(dep_path, "ebin"))
                ctx.actions.run_shell(
                    inputs = [src],
                    outputs = [dest],
                    command = "cp -R {} {}".format(src.path, dest.dirname),
                )
            else:
                dest = ctx.actions.declare_file(path_join(dep_path, "ebin", src.basename))
                ctx.actions.symlink(output = dest, target_file = src)
            erl_libs_files.append(dest)
        for src in lib_info.priv:
            rp = additional_file_dest_relative_path(dep.label, src)
            dest = ctx.actions.declare_file(path_join(dep_path, rp))
            ctx.actions.symlink(output = dest, target_file = src)
            erl_libs_files.append(dest)
    return erl_libs_files

def sname(ctx):
    return sanitize_sname("ct-{}-{}".format(
        ctx.label.package.rpartition("/")[-1],
        ctx.label.name,
    ))

def _impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    erl_libs_files = erl_libs_contents(ctx)

    package = ctx.label.package

    filter_tests_args = ""
    if len(ctx.attr.suites + ctx.attr.groups + ctx.attr.cases) > 0:
        if len(ctx.attr.suites) > 0:
            filter_tests_args = filter_tests_args + " -suite " + " ".join(ctx.attr.suites)
        if len(ctx.attr.groups) > 0:
            filter_tests_args = filter_tests_args + " -group " + " ".join(ctx.attr.groups)
        if len(ctx.attr.cases) > 0:
            filter_tests_args = filter_tests_args + " -case " + " ".join(ctx.attr.cases)

    erl_libs_path = path_join(package, ERL_LIBS_DIR)

    ct_hooks_args = ""
    if len(ctx.attr.ct_hooks) > 0:
        ct_hooks_args = "-ct_hooks " + " ".join(ctx.attr.ct_hooks)

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

FILTER=${{FOCUS:-{filter_tests_args}}}

set -x
"{erlang_home}"/bin/ct_run \\
    -no_auto_compile \\
    -noinput \\
    $FILTER \\
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
            filter_tests_args = filter_tests_args,
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

REM TEST_SRCDIR is provided by bazel but with unix directory separators
set dir=%TEST_SRCDIR%/%TEST_WORKSPACE%/{dir}
set dir=%dir:/=\\%

set logdir=%TEST_UNDECLARED_OUTPUTS_DIR%
set logdir=%logdir:/=\\%
subst b: %logdir%

set ERL_LIBS=%TEST_SRCDIR%/%TEST_WORKSPACE%/{erl_libs_path}
set ERL_LIBS=%ERL_LIBS:/=\\%

{test_env}

if NOT [{package}] == [] cd {package}

if defined FOCUS set FILTER=%FOCUS% else FILTER={filter_tests_args}
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
""".format(
            package = package,
            erlang_home = windows_path(ctx.attr._erlang_home[ErlangHomeProvider].path),
            erlang_version = erlang_version,
            erl_libs_path = erl_libs_path,
            filter_tests_args = filter_tests_args,
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
        files = ctx.files.compiled_suites + ctx.files.data,
        transitive_files = depset(erl_libs_files),
    )
    for tool in ctx.attr.tools:
        runfiles = runfiles.merge(tool[DefaultInfo].default_runfiles)

    return [DefaultInfo(
        runfiles = runfiles,
        executable = output,
    )]

ct_test = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
        "is_windows": attr.bool(mandatory = True),
        "compiled_suites": attr.label_list(
            allow_files = [".beam"],
            mandatory = True,
        ),
        "ct_hooks": attr.string_list(),
        "data": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "tools": attr.label_list(),
        "test_env": attr.string_dict(),
        "suites": attr.string_list(),
        "groups": attr.string_list(),
        "cases": attr.string_list(),
    },
    test = True,
)
