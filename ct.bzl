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
        for d in _unique_short_dirnames(dep[ErlangLibInfo].beam)
    ]

def additional_file_dest_relative_path(dep_label, f):
    if dep_label.workspace_root != "":
        if dep_label.package != "":
            rel_base = path_join(dep_label.workspace_root, dep_label.package)
        else:
            rel_base = dep_label.workspace_root
    else:
        rel_base = dep_label.package
    if rel_base != "":
        return f.path.replace(rel_base + "/", "")
    else:
        return f.path

def _impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    erl_libs_files = []
    for dep in flat_deps(ctx.attr.deps):
        lib_info = dep[ErlangLibInfo]
        dep_path = path_join(ERL_LIBS_DIR, lib_info.lib_name)
        if lib_info.erlang_version != erlang_version:
            fail("Mismatched erlang versions", erlang_version, lib_info.erlang_version)
        for src in lib_info.beam:
            if src.is_directory:
                dest = ctx.actions.declare_directory(path_join(dep_path, "ebin"))
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
                dest = ctx.actions.declare_file(path_join(dep_path, "ebin", src.basename))
                ctx.actions.symlink(output = dest, target_file = src)
            erl_libs_files.append(dest)
        for src in lib_info.priv:
            rp = additional_file_dest_relative_path(dep.label, src)
            dest = ctx.actions.declare_file(path_join(dep_path, rp))
            ctx.actions.symlink(output = dest, target_file = src)
            erl_libs_files.append(dest)

    package = ctx.label.package

    filter_tests_args = ""
    if len(ctx.attr.suites + ctx.attr.groups + ctx.attr.cases) > 0:
        if len(ctx.attr.suites) > 0:
            filter_tests_args = filter_tests_args + " -suite " + " ".join(ctx.attr.suites)
        if len(ctx.attr.groups) > 0:
            filter_tests_args = filter_tests_args + " -group " + " ".join(ctx.attr.groups)
        if len(ctx.attr.cases) > 0:
            filter_tests_args = filter_tests_args + " -case " + " ".join(ctx.attr.cases)

    erl_libs_path = path_join("$TEST_SRCDIR", "$TEST_WORKSPACE")
    if package != "":
        erl_libs_path = path_join(erl_libs_path, package)
    erl_libs_path = path_join(erl_libs_path, ERL_LIBS_DIR)

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

    script = """set -euo pipefail

export HOME=${{TEST_TMPDIR}}

{begins_with_fun}
V=$({erlang_home}/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

export ERL_LIBS={erl_libs_path}

{test_env}

if [ -n "{package}" ]; then
    cd {package}
fi

FILTER=${{FOCUS:-{filter_tests_args}}}

set -x
{erlang_home}/bin/ct_run \\
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
        sname = sname,
        test_env = " && ".join(test_env_commands),
    )

    ctx.actions.write(
        output = ctx.outputs.executable,
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
        "ct_hooks": attr.string_list(),
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
        name_suffix = "",
        suite_name = suite_name,
        deps = deps,
        **kwargs
    )

    return suite_name

def ct_suite_variant(
        name = "",
        name_suffix = "",
        suite_name = "",
        additional_beam = [],
        data = [],
        deps = [],
        runtime_deps = [],
        tools = [],
        test_env = {},
        matrix = [],
        **kwargs):
    if suite_name == "":
        suite_name = name

    data_dir_files = native.glob(["test/{}_data/**/*".format(suite_name)])

    if len(matrix) > 0:
        all_tests = []
        for tag, args in matrix.items():
            test_name = "{}-{}".format(name, tag)
            test_kwargs = dict(**kwargs)
            test_kwargs.update(args)
            ct_test(
                name = test_name + name_suffix,
                compiled_suites = [":{}_beam_files".format(suite_name)] + additional_beam,
                data = data_dir_files + data,
                deps = [":test_bazel_erlang_lib"] + deps + runtime_deps,
                tools = tools,
                test_env = test_env,
                suites = [suite_name],
                **test_kwargs
            )
            all_tests.append(test_name + name_suffix)

        native.test_suite(
            name = name + name_suffix,
            tests = all_tests,
        )
    else:
        ct_test(
            name = name + name_suffix,
            compiled_suites = [":{}_beam_files".format(suite_name)] + additional_beam,
            data = data_dir_files + data,
            deps = [":test_bazel_erlang_lib"] + deps + runtime_deps,
            tools = tools,
            test_env = test_env,
            suites = [suite_name],
            **kwargs
        )

    return suite_name

def ct_group_matrix(groups):
    matrix = {}
    for group in groups:
        matrix[group] = {"groups": [group]}
    return matrix

def ct_group_case_matrix(groups):
    matrix = {}
    for group in groups:
        for case in groups[group]:
            name = "{}-{}".format(group, case)
            matrix[name] = {
                "groups": [group],
                "cases": [case],
            }
    return matrix

def assert_suites(suite_names, suite_files = None):
    if suite_files == None:
        suite_files = native.glob(["test/**/*_SUITE.erl"])
    for f in suite_files:
        sn = f.rpartition("/")[-1].replace(".erl", "")
        if not sn in suite_names:
            fail("A bazel rule has not been defined for {} (expected {} in {}".format(f, sn, suite_names))
