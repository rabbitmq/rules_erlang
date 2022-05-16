load(
    "//private:ct.bzl",
    _code_paths = "code_paths",
    _sanitize_sname = "sanitize_sname",
)
load(
    "//private:util.bzl",
    _additional_file_dest_relative_path = "additional_file_dest_relative_path",
)
load(
    "//private:ct_sharded.bzl",
    "ct_sharded_test",
)
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_INSTALLATION",
    "installation_suffix",
)
load(
    ":erlang_bytecode.bzl",
    "erlang_bytecode",
)
load(
    ":erlang_app.bzl",
    "DEFAULT_TEST_ERLC_OPTS",
)

def additional_file_dest_relative_path(dep_label, f):
    return _additional_file_dest_relative_path(dep_label, f)

def code_paths(ctx, dep):
    return _code_paths(dep)

def sanitize_sname(s):
    return _sanitize_sname(s)

def ct_suite(
        name = "",
        suite_name = "",
        erlang_installations = [DEFAULT_ERLANG_INSTALLATION],
        additional_hdrs = [],
        additional_srcs = [],
        erlc_opts = DEFAULT_TEST_ERLC_OPTS,
        deps = [],
        runtime_deps = [],
        **kwargs):
    if suite_name == "":
        suite_name = name

    for erlang_installation in erlang_installations:
        suffix = installation_suffix(erlang_installation)

        deps_for_this_erlang = [
            "{}-{}".format(dep, suffix)
            for dep in deps
        ]
        runtime_deps_for_this_erlang = [
            "{}-{}".format(dep, suffix)
            for dep in runtime_deps
        ]

        erlang_bytecode(
            name = "{}_beam_files-{}".format(suite_name, suffix),
            erlang_installation = erlang_installation,
            hdrs = native.glob(["include/*.hrl", "src/*.hrl"] + additional_hdrs),
            srcs = ["test/{}.erl".format(suite_name)] + additional_srcs,
            erlc_opts = erlc_opts,
            dest = "test-{}".format(suffix),
            deps = [":test_erlang_app-{}".format(suffix)] + deps_for_this_erlang,
            testonly = True,
        )

        ct_suite_variant(
            name = "{}-{}".format(name, suffix),
            erlang_installation = erlang_installation,
            suite_name = suite_name,
            deps = deps_for_this_erlang,
            runtime_deps = runtime_deps_for_this_erlang,
            **kwargs
        )

    return suite_name

def ct_suite_variant(
        name = "",
        suite_name = "",
        erlang_installation = DEFAULT_ERLANG_INSTALLATION,
        additional_beam = [],
        data = [],
        deps = [],
        runtime_deps = [],
        tags = [],
        **kwargs):
    if suite_name == "":
        suite_name = name

    data_dir_files = native.glob(["test/{}_data/**/*".format(suite_name)])

    suffix = installation_suffix(erlang_installation)

    ct_sharded_test(
        name = name,
        suite_name = suite_name,
        erlang_installation = erlang_installation,
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        compiled_suites = [":{}_beam_files-{}".format(suite_name, suffix)] + additional_beam,
        data = data_dir_files + data,
        deps = [":test_erlang_app-{}".format(suffix)] + deps + runtime_deps,
        tags = tags + [suffix],
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
