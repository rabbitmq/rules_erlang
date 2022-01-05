load(":erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load(":erlang_app.bzl", "DEFAULT_TEST_ERLC_OPTS")
load(":erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load(":erlc.bzl", "erlc")
load(
    ":ct.bzl",
    _assert_suites = "assert_suites",
)
load("//private:ct_sharded.bzl", "ct_sharded_test")

def assert_suites(*args):
    _assert_suites(*args)

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
