load(
    "//private:ct.bzl",
    "ct_test",
    _additional_file_dest_relative_path = "additional_file_dest_relative_path",
    _code_paths = "code_paths",
    _sanitize_sname = "sanitize_sname",
)
load(":erlc.bzl", "erlc")
load(
    ":erlang_app.bzl",
    "DEFAULT_TEST_ERLC_OPTS",
)

def additional_file_dest_relative_path(dep_label, f):
    return _additional_file_dest_relative_path(dep_label, f)

def code_paths(ctx, dep):
    return _code_paths(ctx, dep)

def sanitize_sname(s):
    return _sanitize_sname(s)

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
        tools = [],
        test_env = {},
        **kwargs):
    if suite_name == "":
        suite_name = name

    data_dir_files = native.glob(["test/{}_data/**/*".format(suite_name)])

    ct_test(
        name = name,
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        compiled_suites = [":{}_beam_files".format(suite_name)] + additional_beam,
        data = data_dir_files + data,
        deps = [":test_erlang_app"] + deps + runtime_deps,
        tools = tools,
        test_env = test_env,
        suites = [suite_name],
        **kwargs
    )

    return suite_name

def assert_suites(suite_names, suite_files = None):
    if suite_files == None:
        suite_files = native.glob(["test/**/*_SUITE.erl"])
    for f in suite_files:
        sn = f.rpartition("/")[-1].replace(".erl", "")
        if not sn in suite_names:
            fail("A bazel rule has not been defined for {} (expected {} in {}".format(f, sn, suite_names))
