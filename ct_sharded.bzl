load(":erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load(
    ":bazel_erlang_lib.bzl",
    "BEGINS_WITH_FUN",
    "DEFAULT_TEST_ERLC_OPTS",
    "ErlangLibInfo",
    "QUERY_ERL_VERSION",
    "beam_file",
    "erlc",
    "flat_deps",
    "path_join",
)
load(":ct.bzl", "code_paths", "sanitize_sname", "short_dirname")

def _impl(ctx):
    paths = []
    for dep in flat_deps(ctx.attr.deps):
        paths.extend(code_paths(dep))

    package = ctx.label.package

    pa_args = " ".join([
        "-pa $TEST_SRCDIR/$TEST_WORKSPACE/{}".format(p)
        for p in paths
    ])
    shard_suite_code_paths = ":".join(paths)

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
        export SHARD_SUITE_CODE_PATHS={shard_suite_code_paths}
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
    {pa_args} \\
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
        erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
        pa_args = pa_args,
        shard_suite_code_paths = shard_suite_code_paths,
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
        ctx.files.compiled_suites + ctx.files.data + ctx.files._shard_suite_escript,
    )
    for dep in ctx.attr.deps:
        runfiles = runfiles.merge(dep[DefaultInfo].default_runfiles)
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
        "suite_name": attr.string(mandatory = True),
        "compiled_suites": attr.label_list(
            allow_files = [".beam"],
            mandatory = True,
        ),
        "ct_hooks": attr.string_list(),
        "data": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [ErlangLibInfo]),
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
        deps = [":test_bazel_erlang_lib"] + deps,
        testonly = True,
    )

    ct_suite_variant(
        name = name,
        suite_name = suite_name,
        deps = deps,
        **kwargs
    )

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
        compiled_suites = [":{}_beam_files".format(suite_name)] + additional_beam,
        data = data_dir_files + data,
        deps = [":test_bazel_erlang_lib"] + deps + runtime_deps,
        **kwargs
    )
