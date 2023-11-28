load(
    "//:erlang_app_info.bzl",
    "ErlangAppInfo",
    "flat_deps",
)
load(
    "//:util.bzl",
    "path_join",
    "windows_path",
)
load(
    ":compile_many.bzl",
    "CompileManyInfo",
)
load(
    ":util.bzl",
    "erl_libs_contents",
    "to_erlang_atom_list",
    "to_erlang_string_list",
)
load(
    ":xref2.bzl",
    "XREF_ERL",
    "ALLOWED_SCOPES",
    "SCOPE_PATTERN",
    "EXTRA_APP_DIRS_PATTERN",
    "DEPS_DIRS_PATTERN",
    "APPS_DIRS_PATTERN",
    "TARGET_DIR_PATTERN",
    "EXTRA_DIRS_PATTERN",
    "IGNORE_CALLBACKS_PATTERN",
    "IGNORE_PATTERN",
    "replace_all",
)
load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
)

def _app_path(files):
    for f in files:
        if f.basename.endswith(".beam"):
            parts = f.short_path.split("/")
            return path_join(*parts[:-2])

def _expand_xref_erl(ctx, method = None, arg = None):
    for scope in ctx.attr.scopes:
        if not scope in ALLOWED_SCOPES:
            fail("scope {} is not one of {}", scope, ALLOWED_SCOPES)

    additional_libs_dir = ctx.label.name + "_additional_libs"
    erl_libs_files = erl_libs_contents(
        ctx,
        deps = ctx.attr.additional_libs,
        dir = additional_libs_dir,
    )
    erl_libs_dirs = []
    if len(ctx.attr.additional_libs) > 0:
        erl_libs_dirs.append(path_join(ctx.label.package, additional_libs_dir))

    deps_dirs = []
    for dep in ctx.attr.deps:
        dep_path = None
        for cm in ctx.attr.erl_libs:
            info = cm[CompileManyInfo]
            if dep in info.apps:
                dep_path = _app_path(info.apps[dep].outs)
                for out in info.apps[dep].outs:
                    if out.basename.endswith(".beam"):
                        erl_libs_files.append(out)
        if dep_path == None:
            fail("{} is not found in any of {}".format(dep, ctx.attr.erl_libs))
        (parent, _, _) = dep_path.rpartition("/")
        if parent not in erl_libs_dirs:
            # print("adding {} to erl_libs_dirs".format(parent))
            erl_libs_dirs.append(parent)

    apps_dirs = []
    for app in ctx.attr.apps:
        app_path = None
        for cm in ctx.attr.erl_libs:
            info = cm[CompileManyInfo]
            if app in info.apps:
                app_path = _app_path(info.apps[app].outs)
                for out in info.apps[app].outs:
                    if out.basename.endswith(".beam"):
                        erl_libs_files.append(out)
        if app_path == None:
            fail("{} is not found in any of {}".format(app, ctx.attr.erl_libs))
        (parent, _, _) = app_path.rpartition("/")
        if parent not in erl_libs_dirs:
            # print("adding {} to erl_libs_dirs".format(parent))
            erl_libs_dirs.append(parent)

    extra_dirs = [f.short_path for f in ctx.files.extra_dirs]

    xref_erl = replace_all(XREF_ERL, {
        SCOPE_PATTERN: to_erlang_atom_list(ctx.attr.scopes),
        EXTRA_APP_DIRS_PATTERN: "[]", # to_erlang_string_list(extra_app_dirs),
        DEPS_DIRS_PATTERN: to_erlang_string_list(deps_dirs),
        APPS_DIRS_PATTERN: to_erlang_string_list(apps_dirs),
        TARGET_DIR_PATTERN: ".",
        EXTRA_DIRS_PATTERN: to_erlang_string_list(extra_dirs),
        IGNORE_CALLBACKS_PATTERN: ctx.attr.ignore_callbacks,
        IGNORE_PATTERN: to_erlang_atom_list(ctx.attr.ignore),
        "$1": method,
        "$2": arg,
    }).replace(
        '"',
        '\\"',
    ).replace(
        "\n",
        " ",
    )

    return (
        xref_erl,
        erl_libs_dirs,
        erl_libs_files,
    )

def _impl(ctx):
    (xref_erl, erl_libs_dirs, erl_libs_files) = _expand_xref_erl(
        ctx,
        method = "check",
        arg = to_erlang_atom_list(ctx.attr.checks),
    )

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    if not ctx.attr.is_windows:
        output = ctx.actions.declare_file(ctx.label.name)
        script = """set -euo pipefail

{maybe_install_erlang}

set -x

export HOME=${{TEST_TMPDIR}}
export ERL_LIBS={erl_libs}

tree $ERL_LIBS

if [ -n "{package}" ]; then
    cd {package}
fi

"{erlang_home}"/bin/erl \\
    -noshell \\
    -eval "{xref_erl}" \\
    -pa ebin/
""".format(
            maybe_install_erlang = maybe_install_erlang(ctx, short_path = True),
            erlang_home = erlang_home,
            erl_libs = ctx.configuration.host_path_separator.join([
                path_join("$TEST_SRCDIR", "$TEST_WORKSPACE", d)
                for d in erl_libs_dirs
            ]),
            package = ctx.label.package,
            xref_erl = xref_erl,
        )
    else:
        output = ctx.actions.declare_file(ctx.label.name + ".bat")
        script = """@echo off

set ERL_LIBS={erl_libs}
set ERL_LIBS=%ERL_LIBS:/=\\%

if NOT [{package}] == [] cd {package}

"{erlang_home}\\bin\\erl" ^
    -noshell ^
    -eval "{xref_erl}" ^
    -pa ebin/
""".format(
            erlang_home = windows_path(erlang_home),
            erl_libs = ctx.configuration.host_path_separator.join([
                path_join("%TEST_SRCDIR%", "%TEST_WORKSPACE%", d)
                for d in erl_libs_dirs
            ]),
            xref_erl = xref_erl,
            package = ctx.label.package,
        )

    ctx.actions.write(
        output = output,
        content = script,
    )

    runfiles = runfiles.merge(
        ctx.runfiles(ctx.attr.target[ErlangAppInfo].beam + erl_libs_files + ctx.files.extra_dirs),
    )

    return [DefaultInfo(
        runfiles = runfiles,
        executable = output,
    )]

_XREF_ATTRS = {
    "is_windows": attr.bool(mandatory = True),
    "erl_libs": attr.label_list(
        providers = [CompileManyInfo],
    ),
    "target": attr.label(
        providers = [ErlangAppInfo],
        mandatory = True,
    ),
    "deps": attr.string_list(),
    "apps": attr.string_list(),
    "checks": attr.string_list(
        default = ["undefined_function_calls"],
    ),
    "scopes": attr.string_list(
        default = ["app"],
    ),
    # "extra_apps": attr.label_list(
    #     providers = [ErlangAppInfo],
    # ),
    "extra_dirs": attr.label_list(
        allow_files = True,
    ),
    "ignore": attr.string_list(),
    "ignore_callbacks": attr.string(),
    "additional_libs": attr.label_list(
        providers = [ErlangAppInfo],
    ),
}

xref_test = rule(
    implementation = _impl,
    attrs = _XREF_ATTRS,
    toolchains = ["//tools:toolchain_type"],
    test = True,
)

def _query_impl(ctx):
    (xref_erl, erl_libs_dirs, erl_libs_files) = _expand_xref_erl(
        ctx,
        method = "query",
        arg = "\"$QUERY\"",
    )

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    if not ctx.attr.is_windows:
        output = ctx.actions.declare_file(ctx.label.name)
        script = """set -euo pipefail

{maybe_install_erlang}

export ERL_LIBS={erl_libs}

if [ -n "{package}" ]; then
    cd {package}
fi

export QUERY="$1"

"{erlang_home}"/bin/erl \\
    -noshell \\
    -eval "{xref_erl}" \\
    -pa ebin/
""".format(
            maybe_install_erlang = maybe_install_erlang(ctx, short_path = True),
            erlang_home = erlang_home,
            erl_libs = ctx.configuration.host_path_separator.join([
                path_join("$TEST_SRCDIR", "$TEST_WORKSPACE", d)
                for d in erl_libs_dirs
            ]),
            package = ctx.label.package,
            xref_erl = xref_erl,
        )
    else:
        output = ctx.actions.declare_file(ctx.label.name + ".bat")
        script = """@echo off

set ERL_LIBS={erl_libs}

if NOT [{package}] == [] cd {package}

set QUERY=%1%

"{erlang_home}\\bin\\erl" ^
    -noshell ^
    -eval "{xref_erl}" ^
    -pa ebin/
""".format(
            erlang_home = windows_path(erlang_home),
            erl_libs = ctx.configuration.host_path_separator.join([
                windows_path(path_join("%TEST_SRCDIR%", "%TEST_WORKSPACE%", d))
                for d in erl_libs_dirs
            ]),
            package = ctx.label.package,
            xref_erl = xref_erl,
        )

    ctx.actions.write(
        output = output,
        content = script,
    )

    runfiles = runfiles.merge(
        ctx.runfiles(ctx.attr.target[ErlangAppInfo].beam + erl_libs_files + ctx.files.extra_dirs),
    )

    return [DefaultInfo(
        runfiles = runfiles,
        executable = output,
    )]

xref_query = rule(
    implementation = _query_impl,
    attrs = _XREF_ATTRS,
    toolchains = ["//tools:toolchain_type"],
    executable = True,
)
