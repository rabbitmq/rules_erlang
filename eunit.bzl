load(":erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load(":erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load(":erlang_app.bzl", "DEFAULT_TEST_ERLC_OPTS")
load(":erlc.bzl", "erlc")
load(
    ":util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
)
load(":ct.bzl", "code_paths")

def _to_atom_list(l):
    return "[" + ",".join(["'{}'".format(i) for i in l]) + "]"

def _impl(ctx):
    paths = []
    for dep in flat_deps(ctx.attr.deps):
        paths.extend(code_paths(ctx, dep))

    package = ctx.label.package

    pa_args = " ".join(["-pa $TEST_SRCDIR/$TEST_WORKSPACE/{}".format(p) for p in paths])

    test_env_commands = []
    for k, v in ctx.attr.test_env.items():
        test_env_commands.append("export {}=\"{}\"".format(k, v))

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

# TODO: should we stick the deps in the ERLANG_LIBS instead of pa args?

{erlang_home}/bin/erl +A1 -noinput -boot no_dot_erlang \\
    {pa_args} \\
    -eval "case eunit:test({eunit_mods_term},[]) of ok -> ok; error -> halt(2) end, halt()"
""".format(
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        package = package,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
        erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
        pa_args = pa_args,
        eunit_mods_term = _to_atom_list(ctx.attr.eunit_mods),
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

eunit_test = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = ":erlang_home"),
        "_erlang_version": attr.label(default = ":erlang_version"),
        "compiled_suites": attr.label_list(
            allow_files = [".beam"],
            mandatory = True,
        ),
        "eunit_mods": attr.string_list(mandatory = True),
        "data": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "tools": attr.label_list(),
        "test_env": attr.string_dict(),
    },
    test = True,
)

def _module_name(p):
    return p.rpartition("/")[-1].replace(".erl", "")

def eunit(
        erlc_opts = DEFAULT_TEST_ERLC_OPTS,
        data = [],
        deps = [],
        runtime_deps = [],
        tools = [],
        test_env = {},
        **kwargs):
    srcs = native.glob(["test/**/*.erl"])
    erlc(
        name = "test_case_beam_files",
        hdrs = native.glob(["include/*.hrl", "src/*.hrl"]),
        srcs = srcs,
        erlc_opts = erlc_opts,
        dest = "test",
        deps = [":test_erlang_app"] + deps,
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
        name = "eunit",
        compiled_suites = [":test_case_beam_files"],
        eunit_mods = eunit_mods,
        data = native.glob(["test/**/*"], exclude = srcs) + data,
        deps = [":test_erlang_app"] + deps + runtime_deps,
        tools = tools,
        test_env = test_env,
        **kwargs
    )
