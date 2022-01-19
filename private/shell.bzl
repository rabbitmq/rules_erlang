load("//:erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load("//:erlang_app_info.bzl", "ErlangAppInfo")
load(
    "//:util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
    "path_join",
    "windows_path",
)
load(
    ":ct.bzl",
    "ERL_LIBS_DIR",
    "erl_libs_contents",
)

def _impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    erl_libs_dir = ctx.attr.name + "_deps"

    erl_libs_files = erl_libs_contents(ctx, dir = erl_libs_dir)

    package = ctx.label.package

    erl_libs_path = path_join(package, erl_libs_dir)

    if not ctx.attr.is_windows:
        output = ctx.actions.declare_file(ctx.label.name)
        script = """
set -euo pipefail

{begins_with_fun}
V=$("{erlang_home}"/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

export ERL_LIBS=$PWD/{erl_libs_path}

set -x
"{erlang_home}"/bin/erl {extra_erl_args}
""".format(
            begins_with_fun = BEGINS_WITH_FUN,
            query_erlang_version = QUERY_ERL_VERSION,
            erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
            erlang_version = erlang_version,
            erl_libs_path = erl_libs_path,
            extra_erl_args = " ".join(ctx.attr.extra_erl_args),
        )
    else:
        output = ctx.actions.declare_file(ctx.label.name + ".bat")
        script = """@echo off
echo Erlang Version: {erlang_version}

set ERL_LIBS=%cd%\\{erl_libs_path}

echo on
"{erlang_home}\\bin\\erl" {extra_erl_args} || exit /b 1
""".format(
            erlang_home = windows_path(ctx.attr._erlang_home[ErlangHomeProvider].path),
            erlang_version = erlang_version,
            erl_libs_path = windows_path(erl_libs_path),
            extra_erl_args = " ".join(ctx.attr.extra_erl_args),
        )

    ctx.actions.write(
        output = output,
        content = script,
    )

    runfiles = ctx.runfiles(
        transitive_files = depset(erl_libs_files),
    )

    return [DefaultInfo(
        runfiles = runfiles,
        executable = output,
    )]

shell_private = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
        "is_windows": attr.bool(mandatory = True),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "extra_erl_args": attr.string_list(),
    },
    executable = True,
)
