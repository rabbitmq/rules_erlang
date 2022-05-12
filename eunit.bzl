load("//private:eunit.bzl", "eunit_test")
load(":erlang_app.bzl", "DEFAULT_TEST_ERLC_OPTS")
load(":erlang_bytecode.bzl", "erlang_bytecode")
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_INSTALLATION",
    "installation_suffix",
)

def _module_name(p):
    return p.rpartition("/")[-1].replace(".erl", "")

def eunit(
        erlang_installations = [DEFAULT_ERLANG_INSTALLATION],
        erlc_opts = DEFAULT_TEST_ERLC_OPTS,
        data = [],
        deps = [],
        runtime_deps = [],
        tools = [],
        test_env = {},
        tags = [],
        **kwargs):
    srcs = native.glob(["test/**/*.erl"])
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
            name = "test_case_beam_files-{}".format(suffix),
            erlang_installation = erlang_installation,
            compile_first = compile_first,
            hdrs = native.glob(["include/*.hrl", "src/*.hrl"]),
            srcs = srcs,
            erlc_opts = erlc_opts,
            dest = "test-{}".format(suffix),
            deps = [":test_erlang_app-{}".format(suffix)] + deps_for_this_erlang,
            testonly = True,
        )

        # eunit_mods is the list of source modules, plus any test module which is
        # not amoung the eunit_mods with a "_tests" suffix appended
        eunit_ebin_mods = [_module_name(f) for f in native.glob(["src/**/*.erl"])]
        eunit_test_mods = [_module_name(f) for f in srcs]
        eunit_mods = eunit_ebin_mods
        for tm in eunit_test_mods:
            if tm not in [m + "_tests" for m in eunit_ebin_mods]:
                eunit_mods.append(tm)

        eunit_test(
            name = "eunit-{}".format(suffix),
            erlang_installation = erlang_installation,
            is_windows = select({
                "@bazel_tools//src/conditions:host_windows": True,
                "//conditions:default": False,
            }),
            compiled_suites = [":test_case_beam_files-{}".format(suffix)],
            eunit_mods = eunit_mods,
            data = native.glob(["test/**/*"], exclude = srcs) + data,
            deps = [":test_erlang_app-{}".format(suffix)] + deps_for_this_erlang + runtime_deps_for_this_erlang,
            tools = tools,
            test_env = test_env,
            tags = tags + [suffix],
            **kwargs
        )
