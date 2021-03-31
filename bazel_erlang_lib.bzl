load(":erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")

ErlangLibInfo = provider(
    doc = "Compiled Erlang sources",
    fields = {
        "lib_name": "Name of the erlang lib",
        "erlang_version": "The erlang version used to produce the beam files",
        "include": "Public header files",
        "beam": "Compiled bytecode",
        "priv": "Additional files",
        "deps": "Runtime dependencies of the compiled sources",
    },
)

BEGINS_WITH_FUN = """beginswith() { case $2 in "$1"*) true;; *) false;; esac; }"""
QUERY_ERL_VERSION = """erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(Version), halt().' -noshell"""

DEFAULT_ERLC_OPTS = [
    "-Werror",
    "+debug_info",
    "+warn_export_vars",
    "+warn_shadow_vars",
    "+warn_obsolete_guard",
]

DEFAULT_TEST_ERLC_OPTS = [
    "+debug_info",
    "+warn_export_vars",
    "+warn_shadow_vars",
    "+warn_obsolete_guard",
    "-DTEST=1",
]

# NOTE: we should probably fetch the separator with ctx.host_configuration.host_path_separator
def path_join(*components):
    return "/".join(components)

def _contains_by_lib_name(dep, deps):
    for d in deps:
        if d[ErlangLibInfo].lib_name == dep[ErlangLibInfo].lib_name:
            # TODO: fail if name matches but they are not identical
            return True
    return False

def flat_deps(list_of_labels_providing_erlang_lib_info):
    deps = []
    for dep in list_of_labels_providing_erlang_lib_info:
        if not _contains_by_lib_name(dep, deps):
            deps.append(dep)
            for t in dep[ErlangLibInfo].deps:
                if not _contains_by_lib_name(t, deps):
                    deps.append(t)
    return deps

def unique_dirnames(files):
    dirs = []
    for f in files:
        dirname = f.path if f.is_directory else f.dirname
        if dirname not in dirs:
            dirs.append(dirname)
    return dirs

def _module_name(f):
    return "'{}'".format(f.basename.replace(".beam", "", 1))

def _app_file_impl(ctx):
    app_file = ctx.actions.declare_file(
        path_join("ebin", "{}.app".format(ctx.attr.app_name)),
    )

    if len(ctx.files.app_src) > 1:
        fail("Multiple .app.src files ({}) are not supported".format(ctx.files.app_src))

    modules_list = "[" + ",".join([_module_name(m) for m in ctx.files.modules]) + "]"

    if len(ctx.files.app_src) == 1:
        modules_term = "{modules," + modules_list + "}"

        # TODO: handle the data structure manipulation with erlang itself
        ctx.actions.expand_template(
            template = ctx.files.app_src[0],
            output = app_file,
            substitutions = {
                "{modules,[]}": modules_term,
            },
        )
    else:
        if ctx.attr.app_version == "":
            fail("app_version must be set when app_src is empty")

        if ctx.attr.app_module != "" and len([m for m in ctx.files.modules if m.basename == ctx.attr.app_module + ".beam"]) == 1:
            template = ctx.file._app_with_mod_file_template
        else:
            template = ctx.file._app_file_template

        project_description = ctx.attr.app_description if ctx.attr.app_description != "" else ctx.attr.app_name

        registered_list = "[" + ",".join([ctx.attr.app_name + "_sup"] + ctx.attr.app_registered) + "]"

        applications = ["kernel", "stdlib"] + ctx.attr.extra_apps
        for dep in ctx.attr.deps:
            applications.append(dep[ErlangLibInfo].lib_name)
        applications_list = "[" + ",".join(applications) + "]"

        ctx.actions.expand_template(
            template = template,
            output = app_file,
            substitutions = {
                "$(PROJECT)": ctx.attr.app_name,
                "$(PROJECT_DESCRIPTION)": project_description,
                "$(PROJECT_VERSION)": ctx.attr.app_version,
                "$(PROJECT_ID_TERM)": "",
                "$(MODULES_LIST)": modules_list,
                "$(REGISTERED_LIST)": registered_list,
                "$(APPLICATIONS_LIST)": applications_list,
                "$(PROJECT_MOD)": ctx.attr.app_module,
                "$(PROJECT_ENV)": ctx.attr.app_env,
            },
        )

    return [
        DefaultInfo(files = depset([app_file])),
    ]

app_file = rule(
    implementation = _app_file_impl,
    attrs = {
        "_erlang_home": attr.label(default = ":erlang_home"),
        "_erlang_version": attr.label(default = ":erlang_version"),
        "_app_file_template": attr.label(
            default = Label("//:app_file.template"),
            allow_single_file = True,
        ),
        "_app_with_mod_file_template": attr.label(
            default = Label("//:app_with_mod_file.template"),
            allow_single_file = True,
        ),
        "app_name": attr.string(mandatory = True),
        "app_version": attr.string(),
        "app_description": attr.string(),
        "app_module": attr.string(),
        "app_registered": attr.string_list(),
        "app_env": attr.string(default = "[]"),
        "extra_apps": attr.string_list(),
        "app_src": attr.label_list(allow_files = [".app.src"]),
        "modules": attr.label_list(allow_files = [".beam"]),
        "deps": attr.label_list(providers = [ErlangLibInfo]),
    },
)

def beam_file(ctx, src, dir):
    name = src.basename.replace(".erl", ".beam")
    return ctx.actions.declare_file(path_join(dir, name))

def _erlc_impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    beam_files = [beam_file(ctx, src, ctx.attr.dest) for src in ctx.files.srcs]

    dest_dir = beam_files[0].dirname

    erl_args = ctx.actions.args()
    erl_args.add("-v")

    for dir in unique_dirnames(ctx.files.hdrs):
        erl_args.add("-I", dir)

    for dep in ctx.attr.deps:
        lib_info = dep[ErlangLibInfo]
        if lib_info.erlang_version != erlang_version:
            fail("Mismatched erlang versions", erlang_version, lib_info.erlang_version)
        for dir in unique_dirnames(lib_info.include):
            erl_args.add("-I", path_join(dir, "../.."))
        for dir in unique_dirnames(lib_info.beam):
            erl_args.add("-pa", dir)

    for dir in unique_dirnames(ctx.files.beam):
        erl_args.add("-pa", dir)

    erl_args.add("-o", dest_dir)

    erl_args.add_all(ctx.attr.erlc_opts)

    erl_args.add_all(ctx.files.srcs)

    script = """
        set -euo pipefail

        mkdir -p {dest_dir}
        export HOME=$PWD

        {begins_with_fun}
        V=$({erlang_home}/bin/{query_erlang_version})
        if ! beginswith "{erlang_version}" "$V"; then
            echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
            exit 1
        fi

        {erlang_home}/bin/erlc $@
    """.format(
        dest_dir = dest_dir,
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        erlang_version = erlang_version,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
    )

    inputs = []
    inputs.extend(ctx.files.hdrs)
    inputs.extend(ctx.files.srcs)
    for dep in ctx.attr.deps:
        lib_info = dep[ErlangLibInfo]
        inputs.extend(lib_info.include)
        inputs.extend(lib_info.beam)
    inputs.extend(ctx.files.beam)

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = beam_files,
        command = script,
        arguments = [erl_args],
        mnemonic = "ERLC",
    )

    return [
        DefaultInfo(files = depset(beam_files)),
    ]

erlc = rule(
    implementation = _erlc_impl,
    attrs = {
        "_erlang_home": attr.label(default = ":erlang_home"),
        "_erlang_version": attr.label(default = ":erlang_version"),
        "hdrs": attr.label_list(allow_files = [".hrl"]),
        "srcs": attr.label_list(allow_files = [".erl"]),
        "beam": attr.label_list(allow_files = [".beam"]),
        "deps": attr.label_list(providers = [ErlangLibInfo]),
        "erlc_opts": attr.string_list(),
        "dest": attr.string(
            default = "ebin",
        ),
    },
)

def _impl(ctx):
    compiled_files = ctx.files.app + ctx.files.beam

    deps = flat_deps(ctx.attr.deps)

    runfiles = ctx.runfiles(compiled_files + ctx.files.priv)
    for dep in ctx.attr.deps:
        runfiles = runfiles.merge(dep[DefaultInfo].default_runfiles)

    return [
        ErlangLibInfo(
            lib_name = ctx.attr.app_name,
            erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
            include = ctx.files.hdrs,
            beam = compiled_files,
            priv = ctx.files.priv,
            deps = deps,
        ),
        DefaultInfo(
            files = depset(compiled_files),
            runfiles = runfiles,
        ),
    ]

bazel_erlang_lib = rule(
    implementation = _impl,
    attrs = {
        "_erlang_version": attr.label(default = ":erlang_version"),
        "app_name": attr.string(mandatory = True),
        "hdrs": attr.label_list(allow_files = [".hrl"]),
        "app": attr.label(allow_files = [".app"]),
        "beam": attr.label_list(allow_files = [".beam"]),
        "priv": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [ErlangLibInfo]),
    },
)

def erlang_lib(
        app_name = "",
        app_version = "",
        app_description = "",
        app_module = "",
        app_registered = [],
        app_env = "[]",
        extra_apps = [],
        erlc_opts = DEFAULT_ERLC_OPTS,
        first_srcs = [],
        build_deps = [],
        deps = [],
        runtime_deps = []):
    all_beam = []

    if len(first_srcs) > 0:
        all_beam = [":first_beam_files"]
        erlc(
            name = "first_beam_files",
            hdrs = native.glob(["include/**/*.hrl", "src/**/*.hrl"]),
            srcs = native.glob(first_srcs),
            erlc_opts = _unique(erlc_opts),
            dest = "ebin",
            deps = build_deps + deps,
        )

    erlc(
        name = "beam_files",
        hdrs = native.glob(["include/**/*.hrl", "src/**/*.hrl"]),
        srcs = native.glob(["src/**/*.erl"], exclude = first_srcs),
        beam = all_beam,
        erlc_opts = _unique(erlc_opts),
        dest = "ebin",
        deps = build_deps + deps,
    )

    all_beam = all_beam + [":beam_files"]

    if len(native.glob(["ebin/{}.app".format(app_name)])) == 0:
        app_file(
            name = "app_file",
            app_name = app_name,
            app_version = app_version,
            app_description = app_description,
            app_module = app_module,
            app_registered = app_registered,
            app_env = app_env,
            extra_apps = extra_apps,
            app_src = native.glob(["src/{}.app.src".format(app_name)]),
            modules = all_beam,
            deps = deps + runtime_deps,
        )
        app = ":app_file"
    else:
        app = "ebin/{}.app".format(app_name)

    bazel_erlang_lib(
        name = "bazel_erlang_lib",
        app_name = app_name,
        hdrs = native.glob(["include/**/*.hrl"]),
        app = app,
        beam = all_beam,
        priv = native.glob(["priv/**/*"]),
        deps = deps + runtime_deps,
        visibility = ["//visibility:public"],
    )

def _unique(l):
    r = []
    for item in l:
        if item not in r:
            r.append(item)
    return r

def test_erlang_lib(
        app_name = "",
        app_version = "",
        app_description = "",
        app_module = "",
        app_registered = [],
        app_env = "[]",
        extra_apps = [],
        erlc_opts = DEFAULT_TEST_ERLC_OPTS,
        first_srcs = [],
        build_deps = [],
        deps = [],
        runtime_deps = []):
    all_beam = []

    if len(first_srcs) > 0:
        all_beam = [":first_test_beam_files"]
        erlc(
            name = "first_test_beam_files",
            hdrs = native.glob(["include/**/*.hrl", "src/**/*.hrl"]),
            srcs = native.glob(first_srcs),
            erlc_opts = _unique(erlc_opts),
            dest = "test",
            deps = build_deps + deps,
            testonly = True,
        )

    erlc(
        name = "test_beam_files",
        hdrs = native.glob(["include/**/*.hrl", "src/**/*.hrl"]),
        srcs = native.glob(["src/**/*.erl"], exclude = first_srcs),
        beam = all_beam,
        erlc_opts = _unique(erlc_opts),
        dest = "test",
        deps = build_deps + deps,
        testonly = True,
    )

    all_beam = all_beam + [":test_beam_files"]

    if len(native.glob(["ebin/{}.app".format(app_name)])) == 0:
        app = ":app_file"
    else:
        app = "ebin/{}.app".format(app_name)

    bazel_erlang_lib(
        name = "test_bazel_erlang_lib",
        app_name = app_name,
        hdrs = native.glob(["include/**/*.hrl"]),
        app = app,
        beam = all_beam,
        priv = native.glob(["priv/**/*"]),
        deps = deps + runtime_deps,
        visibility = ["//visibility:public"],
        testonly = True,
    )
