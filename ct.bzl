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

def sanitize_sname(s):
    return s.replace("@", "-").replace(".", "_")

def _short_dirname(f):
    if f.is_directory:
        return f.short_path
    else:
        return f.short_path.rpartition("/")[0]

def _unique_short_dirnames(files):
    dirs = []
    for f in files:
        dirname = _short_dirname(f)
        if dirname not in dirs:
            dirs.append(dirname)
    return dirs

def code_paths(dep):
    return [
        path_join(dep.label.workspace_root, d) if dep.label.workspace_root != "" else d
        for d in _unique_short_dirnames(dep[ErlangLibInfo].beam)
    ]

def _impl(ctx):
    paths = []
    for dep in flat_deps(ctx.attr.deps):
        paths.extend(code_paths(dep))

    package = ctx.label.package

    pa_args = " ".join(["-pa $TEST_SRCDIR/$TEST_WORKSPACE/{}".format(p) for p in paths])

    filter_tests_args = ""
    if len(ctx.attr.suites + ctx.attr.groups + ctx.attr.cases) > 0:
        if len(ctx.attr.suites) > 0:
            filter_tests_args = filter_tests_args + " -suite " + " ".join(ctx.attr.suites)
        if len(ctx.attr.groups) > 0:
            filter_tests_args = filter_tests_args + " -group " + " ".join(ctx.attr.groups)
        if len(ctx.attr.cases) > 0:
            filter_tests_args = filter_tests_args + " -case " + " ".join(ctx.attr.cases)

    test_env_commands = []
    for k, v in ctx.attr.test_env.items():
        test_env_commands.append("export {}=\"{}\"".format(k, v))

    sname = sanitize_sname("ct-{}-{}".format(
        ctx.label.package.rpartition("/")[-1],
        ctx.label.name,
    ))

    script = """set -euxo pipefail

    export HOME=${{TEST_TMPDIR}}

    {begins_with_fun}
    V=$({erlang_home}/bin/{query_erlang_version})
    if ! beginswith "{erlang_version}" "$V"; then
        echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
        exit 1
    fi

    {test_env}

    cd {package}

    {erlang_home}/bin/ct_run \\
        -no_auto_compile \\
        -noinput \\
        {pa_args}{filter_tests_args} \\
        -dir $TEST_SRCDIR/$TEST_WORKSPACE/{dir} \\
        -logdir ${{TEST_UNDECLARED_OUTPUTS_DIR}} \\
        -sname {sname}
    """.format(
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        package = package,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
        erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
        pa_args = pa_args,
        filter_tests_args = filter_tests_args,
        dir = _short_dirname(ctx.files.compiled_suites[0]),
        sname = sname,
        test_env = " && ".join(test_env_commands),
    )

    ctx.actions.write(
        output = ctx.outputs.executable,
        content = script,
    )

    runfiles = ctx.runfiles(files = ctx.files.compiled_suites + ctx.files.data)
    for dep in ctx.attr.deps:
        runfiles = runfiles.merge(dep[DefaultInfo].default_runfiles)
    for tool in ctx.attr.tools:
        runfiles = runfiles.merge(tool[DefaultInfo].default_runfiles)

    return [DefaultInfo(
        runfiles = runfiles,
    )]

ct_test = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = ":erlang_home"),
        "_erlang_version": attr.label(default = ":erlang_version"),
        "compiled_suites": attr.label_list(
            allow_files = [".beam"],
            mandatory = True,
        ),
        "data": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [ErlangLibInfo]),
        "tools": attr.label_list(),
        "test_env": attr.string_dict(),
        "suites": attr.string_list(),
        "groups": attr.string_list(),
        "cases": attr.string_list(),
    },
    test = True,
)

def ct_suite(
        suite_name = "",
        additional_hdrs = [],
        additional_srcs = [],
        additional_beam = [],
        erlc_opts = DEFAULT_TEST_ERLC_OPTS,
        data = [],
        deps = [],
        runtime_deps = [],
        tools = [],
        test_env = {},
        groups = [],
        **kwargs):
    erlc(
        name = "{}_beam_files".format(suite_name),
        hdrs = native.glob(["include/*.hrl", "src/*.hrl"] + additional_hdrs),
        srcs = ["test/{}.erl".format(suite_name)] + additional_srcs,
        erlc_opts = erlc_opts,
        dest = "test",
        deps = [":test_bazel_erlang_lib"] + deps,
        testonly = True,
    )

    data_dir_files = native.glob(["test/{}_data/**/*".format(suite_name)])

    if len(groups) > 1:
        for group in groups:
            ct_test(
                name = "{}-{}".format(suite_name, group),
                compiled_suites = [":{}_beam_files".format(suite_name)] + additional_beam,
                data = data_dir_files + data,
                deps = [":test_bazel_erlang_lib"] + deps + runtime_deps,
                tools = tools,
                test_env = test_env,
                suites = [suite_name],
                groups = [group],
                **kwargs
            )

        native.test_suite(
            name = suite_name,
            tests = ["{}-{}".format(suite_name, group) for group in groups],
        )
    else:
        ct_test(
            name = suite_name,
            compiled_suites = [":{}_beam_files".format(suite_name)] + additional_beam,
            data = data_dir_files + data,
            deps = [":test_bazel_erlang_lib"] + deps + runtime_deps,
            tools = tools,
            test_env = test_env,
            **kwargs
        )
