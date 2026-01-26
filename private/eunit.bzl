load(
    "//:erlang_app_info.bzl",
    "ErlangAppInfo",
)
load(
    "//:util.bzl",
    "path_join",
)
load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
)
load(
    ":util.bzl",
    "erl_libs_contents",
    "to_erlang_string_list",
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

def _to_atom_list(atoms):
    return "[" + ",".join(["'%s'" % a for a in atoms]) + "]"

def _quote(string_list_term):
    return string_list_term.replace('"', '\\"')

def _impl(ctx):
    deps = list(ctx.attr.deps)
    lib_info = ctx.attr.target[ErlangAppInfo]
    deps.extend(lib_info.deps)
    deps.append(ctx.attr.target)

    # Use the eunit_mods attribute if provided, otherwise calculate from test_beam
    # Note: EUnit auto-discovers tests in source modules when running the corresponding *_tests module
    # So we only include *_tests modules in the test list
    if ctx.attr.eunit_mods:
        eunit_mods = list(ctx.attr.eunit_mods)
    else:
        eunit_mods = []
        # Include *_tests modules from test_beam
        for m in lib_info.test_beam:
            if m.extension == "beam":
                module_name = m.basename.removesuffix(".beam")
                if module_name.endswith("_tests"):
                    eunit_mods.append(module_name)

    erl_libs_dir = ctx.label.name + "_deps"

    erl_libs_files = erl_libs_contents(
        ctx,
        deps = deps,
        ez_deps = ctx.files.ez_deps,
        dir = erl_libs_dir,
    )

    package = ctx.label.package

    erl_libs_path = path_join(package, erl_libs_dir)

    apps_ebin_dirs = []
    if ctx.configuration.coverage_enabled:
        for dep in deps:
            if dep.label.workspace_name == "":
                apps_ebin_dirs.append(path_join(
                    "$TEST_SRCDIR",
                    "$TEST_WORKSPACE",
                    erl_libs_path,
                    dep[ErlangAppInfo].app_name,
                    "ebin",
                ))

    (erlang_home, _, runfiles) = erlang_dirs(ctx, short_path = True)

    eunit_opts_term = "[" + ",".join(ctx.attr.eunit_opts) + "]"

    coverdata_to_lcov = ctx.attr.coverdata_to_lcov
    coverdata_to_lcov_path = coverdata_to_lcov[DefaultInfo].files_to_run.executable.short_path

    test_env_commands = []
    for k, v in ctx.attr.test_env.items():
        test_env_commands.append("export {}=\"{}\"".format(k, v))

    output = ctx.actions.declare_file(ctx.label.name)
    script = """\
#!/usr/bin/env bash
set -eo pipefail

{maybe_install_erlang}

export HOME=${{TEST_TMPDIR}}
if [ -n "{erl_libs_path}" ]; then
    export ERL_LIBS=$TEST_SRCDIR/$TEST_WORKSPACE/{erl_libs_path}
fi

{test_env}

if [ -n "{package}" ]; then
    cd {package}
fi

COVER_PRE=
COVER_POST=
if [ -n "${{COVERAGE}}" ]; then
    COVER_PRE="[case cover:compile_beam_directory(D) of {{error, _}} -> halt(1); _ -> ok end || D <- {apps_ebin_dirs_term}], "
    # Export coverdata to TEST_UNDECLARED_OUTPUTS_DIR so it persists after the test
    COVER_POST="cover:export(\\"${{TEST_UNDECLARED_OUTPUTS_DIR}}/all.coverdata\\"), "
fi
set -x
"{erlang_home}"/bin/erl +A1 -noinput -boot no_dot_erlang \\
    {extra_args} \\
    -eval "${{COVER_PRE}}case eunit:test({eunit_mods_term},{eunit_opts_term}) of ok -> ok; error -> halt(2) end, ${{COVER_POST}}halt()."
set +x
if [ -n "${{COVERAGE}}" ]; then
    # Convert the persisted coverdata to LCOV format for Bazel's coverage report
    "{erlang_home}"/bin/escript $TEST_SRCDIR/$TEST_WORKSPACE/{coverdata_to_lcov} \\
        ${{TEST_UNDECLARED_OUTPUTS_DIR}}/all.coverdata \\
        ${{COVERAGE_OUTPUT_FILE}} \\
        > ${{TEST_UNDECLARED_OUTPUTS_DIR}}/coverdata_to_lcov.log
fi
""".format(
        maybe_install_erlang = maybe_install_erlang(ctx, short_path = True),
        erlang_home = erlang_home,
        apps_ebin_dirs_term = _quote(to_erlang_string_list(apps_ebin_dirs)),
        erl_libs_path = erl_libs_path if len(erl_libs_files) > 0 else "",
        package = package,
        coverdata_to_lcov = coverdata_to_lcov_path,
        extra_args = " ".join(ctx.attr.erl_extra_args),
        eunit_mods_term = _to_atom_list(eunit_mods),
        eunit_opts_term = eunit_opts_term,
        test_env = "\n".join(test_env_commands),
    )

    ctx.actions.write(
        output = output,
        content = script,
    )

    runfiles = runfiles.merge_all(
        [ctx.runfiles(
            files = lib_info.test_data,
            transitive_files = depset(erl_libs_files),
        )] + ([coverdata_to_lcov[DefaultInfo].default_runfiles] if ctx.configuration.coverage_enabled else []) + [
            tool[DefaultInfo].default_runfiles
            for tool in ctx.attr.tools
        ],
    )
    if ctx.attr.target != None:
        runfiles = runfiles.merge(ctx.attr.target[DefaultInfo].default_runfiles)

    return [DefaultInfo(
        runfiles = runfiles,
        executable = output,
    )]

eunit_test = rule(
    implementation = _impl,
    attrs = {
        "coverdata_to_lcov": attr.label(
            executable = True,
            cfg = "target",
        ),
        "eunit_mods": attr.string_list(),
        "target": attr.label(providers = [ErlangAppInfo]),
        "erl_extra_args": attr.string_list(),
        "eunit_opts": attr.string_list(),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "ez_deps": attr.label_list(
            allow_files = [".ez"],
        ),
        "tools": attr.label_list(),
        "test_env": attr.string_dict(),
    },
    toolchains = ["//tools:toolchain_type"],
    test = True,
)
