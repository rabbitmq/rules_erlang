load(":erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load(":erlang_app.bzl", "DEFAULT_TEST_ERLC_OPTS")
load(":erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load(":erlc.bzl", "erlc")
load(
    ":ct.bzl",
    "ERL_LIBS_DIR",
    "additional_file_dest_relative_path",
    "sanitize_sname",
    "short_dirname",
    _assert_suites = "assert_suites",
)
load("//private:erlc.bzl", "beam_file")
load(
    ":util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
    "path_join",
)

def _impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    erl_libs_files = []
    for dep in flat_deps(ctx.attr.deps):
        lib_info = dep[ErlangAppInfo]
        dep_path = path_join(ctx, ERL_LIBS_DIR, lib_info.lib_name)
        if lib_info.erlang_version != erlang_version:
            fail("Mismatched erlang versions", erlang_version, lib_info.erlang_version)
        for src in lib_info.beam:
            if src.is_directory:
                dest = ctx.actions.declare_directory(path_join(ctx, dep_path, "ebin"))
                args = ctx.actions.args()
                args.add_all([src])
                args.add(dest.path)
                ctx.actions.run(
                    inputs = [src],
                    outputs = [dest],
                    executable = "cp",
                    arguments = [args],
                )
            else:
                dest = ctx.actions.declare_file(path_join(ctx, dep_path, "ebin", src.basename))
                ctx.actions.symlink(output = dest, target_file = src)
            erl_libs_files.append(dest)
        for src in lib_info.priv:
            rp = additional_file_dest_relative_path(dep.label, src)
            dest = ctx.actions.declare_file(path_join(ctx, dep_path, rp))
            ctx.actions.symlink(output = dest, target_file = src)
            erl_libs_files.append(dest)

    package = ctx.label.package

    erl_libs_path = path_join(ctx, "$TEST_SRCDIR", "$TEST_WORKSPACE")
    if package != "":
        erl_libs_path = path_join(ctx, erl_libs_path, package)
    erl_libs_path = path_join(ctx, erl_libs_path, ERL_LIBS_DIR)

    test_env_commands = []
    for k, v in ctx.attr.test_env.items():
        test_env_commands.append("export {}=\"{}\"".format(k, v))

    ct_hooks_args = ""
    if len(ctx.attr.ct_hooks) > 0:
        ct_hooks_args = "-ct_hooks " + " ".join(ctx.attr.ct_hooks)

    sname = sanitize_sname("ct-{}-{}".format(
        ctx.label.package.rpartition("/")[-1],
        ctx.label.name,
    ))

    script = """set -eo pipefail

if [ -n "${{TEST_SHARD_STATUS_FILE+x}}" ]; then
    touch ${{TEST_SHARD_STATUS_FILE}}
fi

export HOME=${{TEST_TMPDIR}}

{begins_with_fun}
V=$({erlang_home}/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

export ERL_LIBS={erl_libs_path}

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
        FILTER=$({erlang_home}/bin/escript \\
            $TEST_SRCDIR/$TEST_WORKSPACE/{shard_suite} \\
                -{sharding_method} \\
                {suite_name} ${{TEST_SHARD_INDEX}} ${{TEST_TOTAL_SHARDS}})
    else
        FILTER="-suite {suite_name}"
    fi
fi

if [ -n "{package}" ]; then
    cd {package}
fi

set -x
{erlang_home}/bin/ct_run \\
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
        sname = sname,
        test_env = " && ".join(test_env_commands),
    )

    ctx.actions.write(
        output = ctx.outputs.executable,
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
    )]

ct_sharded_test = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = ":erlang_home"),
        "_erlang_version": attr.label(default = ":erlang_version"),
        "_shard_suite_escript": attr.label(
            default = "//shard_suite:escript",
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

def ct_suite(
        name = "",
        suite_name = "",
        additional_hdrs = [],
        additional_srcs = [],
        erlc_opts = DEFAULT_TEST_ERLC_OPTS,
        deps = [],
        **kwargs):
    if suite_name == "":
        suite_name = name

    erlc(
        name = "{}_beam_files".format(suite_name),
        hdrs = native.glob(["include/*.hrl", "src/*.hrl"] + additional_hdrs),
        srcs = ["test/{}.erl".format(suite_name)] + additional_srcs,
        erlc_opts = erlc_opts,
        dest = "test",
        deps = [":test_erlang_app"] + deps,
        testonly = True,
    )

    ct_suite_variant(
        name = name,
        suite_name = suite_name,
        deps = deps,
        **kwargs
    )

    return suite_name

def ct_suite_variant(
        name = "",
        suite_name = "",
        additional_beam = [],
        data = [],
        deps = [],
        runtime_deps = [],
        **kwargs):
    if suite_name == "":
        suite_name = name

    data_dir_files = native.glob(["test/{}_data/**/*".format(suite_name)])

    ct_sharded_test(
        name = name,
        suite_name = suite_name,
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        compiled_suites = [":{}_beam_files".format(suite_name)] + additional_beam,
        data = data_dir_files + data,
        deps = [":test_erlang_app"] + deps + runtime_deps,
        **kwargs
    )

    return suite_name

def assert_suites(*args):
    _assert_suites(*args)
