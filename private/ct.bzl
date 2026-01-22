load(
    "@bazel_skylib//rules:common_settings.bzl",
    "BuildSettingInfo",
)
load(
    "//:erlang_app_info.bzl",
    "ErlangAppInfo",
    "flat_deps",
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
    ":eunit.bzl",
    "package_relative_dirnames",
    "short_dirname",
)
load(
    ":util.bzl",
    "erl_libs_contents",
    "to_erlang_string_list",
)

def sanitize_sname(s):
    return s.replace("@", "-").replace(".", "_")

def unique_short_dirnames(files):
    dirs = []
    for f in files:
        dirname = short_dirname(f)
        if dirname not in dirs:
            dirs.append(dirname)
    return dirs

def code_paths(dep):
    return [
        path_join(dep.label.workspace_root, d) if dep.label.workspace_root != "" else d
        for d in unique_short_dirnames(dep[ErlangAppInfo].beam)
    ]

# Calling ctx.expand_location with short_paths=True gives
# "Error in expand_location: Rule in 'private' cannot use private API"
def _expand_locations_short_paths(ctx, s):
    expanded = ctx.expand_location(s, [])
    if expanded != s:
        return expanded.replace(ctx.bin_dir.path, "$TEST_SRCDIR/$TEST_WORKSPACE")
    return s

def sname(ctx):
    return sanitize_sname("ct-{}-{}".format(
        ctx.label.package.rpartition("/")[-1],
        ctx.label.name,
    ))

def _impl(ctx):
    erl_libs_dir = ctx.label.name + "_deps"

    erl_libs_files = erl_libs_contents(
        ctx,
        deps = flat_deps(ctx.attr.deps + ctx.attr.compiled_suites),
        ez_deps = ctx.files.ez_deps,
        dir = erl_libs_dir,
    )

    package = ctx.label.package

    erl_libs_path = path_join(package, erl_libs_dir)

    pa_args = []
    for dir in package_relative_dirnames(package, ctx.files.compiled_suites):
        if dir != "test":
            pa_args.extend(["-pa", dir])

    ct_logdir = ctx.attr._ct_logdir[BuildSettingInfo].value

    extra_args = []
    if len(ctx.attr.ct_hooks) > 0:
        extra_args.append("-ct_hooks " + " ".join(ctx.attr.ct_hooks))
    extra_args.extend(ctx.attr.ct_run_extra_args)

    app_name = ctx.attr.app_name if ctx.attr.app_name != "" else ctx.label.package.rpartition("/")[-1]

    apps_ebin_dirs = []
    if ctx.configuration.coverage_enabled:
        for dep in flat_deps(ctx.attr.deps):
            if dep.label.workspace_name == "":
                apps_ebin_dirs.append(path_join(
                    "$TEST_SRCDIR",
                    "$TEST_WORKSPACE",
                    erl_libs_path,
                    dep[ErlangAppInfo].app_name,
                    "ebin",
                ))

    (erlang_home, _, runfiles) = erlang_dirs(ctx, short_path = True)

    shard_suite = ctx.attr.shard_suite
    shard_suite_path = shard_suite[DefaultInfo].files_to_run.executable.short_path

    coverdata_to_lcov = ctx.attr.coverdata_to_lcov
    coverdata_to_lcov_path = coverdata_to_lcov[DefaultInfo].files_to_run.executable.short_path

    test_env_commands = []
    for k, v in ctx.attr.test_env.items():
        test_env_commands.append("export {}=\"{}\"".format(k, _expand_locations_short_paths(ctx, v)))

    log_dir = ct_logdir if ct_logdir != "" else "${TEST_UNDECLARED_OUTPUTS_DIR}"

    output = ctx.actions.declare_file(ctx.label.name)
    script = """\
#!/usr/bin/env bash
set -eo pipefail

{maybe_install_erlang}

COVER_ARGS=
if [ -n "${{COVERAGE}}" ]; then
    cat << EOF > "${{TEST_UNDECLARED_OUTPUTS_DIR}}/ct.cover.spec"
{{incl_app, '{app_name}', details}}.
{{incl_dirs, {apps_ebin_dirs_term}}}.
{{export, "${{COVERAGE_OUTPUT_FILE}}"}}.
EOF

    COVER_ARGS="-cover ${{TEST_UNDECLARED_OUTPUTS_DIR}}/ct.cover.spec"
fi

if [ -n "{shard_suite}" ]; then
    if [ -n "${{TEST_SHARD_STATUS_FILE+x}}" ]; then
        SHARDING=true
        touch ${{TEST_SHARD_STATUS_FILE}}
    fi
fi

export HOME=${{TEST_TMPDIR}}
export ERL_LIBS=$TEST_SRCDIR/$TEST_WORKSPACE/{erl_libs_path}

{test_env}

if [ -n "${{FOCUS+x}}" ]; then
    if [ -n "${{SHARDING+x}}" ]; then
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
    if [ -n "${{SHARDING+x}}" ]; then
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

mkdir -p "{log_dir}"

set -x
"{erlang_home}"/bin/ct_run \\
    -no_auto_compile \\
    -noinput \\
    ${{FILTER}} \\
    -dir ebin {pa_args} \\
    -logdir "{log_dir}" \\
    -hidden \\
    -sname {sname} ${{COVER_ARGS}} {extra_args}
set +x
if [ -n "${{COVERAGE}}" ]; then
    "{erlang_home}"/bin/escript $TEST_SRCDIR/$TEST_WORKSPACE/{coverdata_to_lcov} \\
        ${{COVERAGE_OUTPUT_FILE}} \\
        ${{COVERAGE_OUTPUT_FILE}} \\
        > ${{TEST_UNDECLARED_OUTPUTS_DIR}}/coverdata_to_lcov.log
fi
""".format(
        maybe_install_erlang = maybe_install_erlang(ctx, short_path = True),
        app_name = app_name,
        apps_ebin_dirs_term = to_erlang_string_list(apps_ebin_dirs),
        erlang_home = erlang_home,
        package = package,
        erl_libs_path = erl_libs_path,
        shard_suite = shard_suite_path,
        sharding_method = ctx.attr.sharding_method,
        coverdata_to_lcov = coverdata_to_lcov_path,
        suite_name = ctx.attr.suite_name,
        pa_args = " ".join(pa_args),
        dir = path_join(package, "test"),
        log_dir = log_dir,
        sname = sname(ctx),
        extra_args = " ".join(extra_args),
        test_env = "\n".join(test_env_commands),
    )

    ctx.actions.write(
        output = output,
        content = script,
    )

    runfiles = runfiles.merge_all(
        [
            ctx.runfiles(ctx.files.compiled_suites + ctx.files.data + erl_libs_files),
            shard_suite[DefaultInfo].default_runfiles,
        ] + ([coverdata_to_lcov[DefaultInfo].default_runfiles] if ctx.configuration.coverage_enabled else []) + [
            tool[DefaultInfo].default_runfiles
            for tool in ctx.attr.tools
        ],
    )

    return [DefaultInfo(
        runfiles = runfiles,
        executable = output,
    )]

ct_test = rule(
    implementation = _impl,
    attrs = {
        "_ct_logdir": attr.label(
            default = Label("//:ct_logdir"),
        ),
        "shard_suite": attr.label(
            executable = True,
            cfg = "target",
        ),
        "coverdata_to_lcov": attr.label(
            executable = True,
            cfg = "target",
        ),
        "app_name": attr.string(),  # should be mandatory
        "suite_name": attr.string(mandatory = True),
        "compiled_suites": attr.label_list(
            allow_files = [".beam"],
            mandatory = True,
        ),
        "ct_hooks": attr.string_list(),
        "ct_run_extra_args": attr.string_list(),
        "data": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "ez_deps": attr.label_list(
            allow_files = [".ez"],
        ),
        "tools": attr.label_list(cfg = "target"),
        "test_env": attr.string_dict(),
        "sharding_method": attr.string(
            default = "group",
            values = ["group", "case"],
        ),
    },
    toolchains = ["//tools:toolchain_type"],
    test = True,
)
