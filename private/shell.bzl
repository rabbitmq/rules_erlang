load("//:erlang_app_info.bzl", "ErlangAppInfo")
load(
    "//:util.bzl",
    "path_join",
    "windows_path",
)
load(":util.bzl", "erl_libs_contents")
load(
    ":erlang_installation.bzl",
    "ErlangInstallationInfo",
    "erlang_dirs",
    "maybe_symlink_erlang",
)

def _impl(ctx):
    erl_libs_dir = ctx.attr.name + "_deps"

    erl_libs_files = erl_libs_contents(ctx, dir = erl_libs_dir)

    package = ctx.label.package

    erl_libs_path = path_join(package, erl_libs_dir)

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    if not ctx.attr.is_windows:
        output = ctx.actions.declare_file(ctx.label.name)
        script = """set -euo pipefail

{maybe_symlink_erlang}

export ERL_LIBS=$PWD/{erl_libs_path}

set -x
"{erlang_home}"/bin/erl {extra_erl_args} $@
""".format(
            maybe_symlink_erlang = maybe_symlink_erlang(ctx, short_path = True),
            erlang_home = erlang_home,
            erl_libs_path = erl_libs_path,
            extra_erl_args = " ".join(ctx.attr.extra_erl_args),
        )
    else:
        output = ctx.actions.declare_file(ctx.label.name + ".bat")
        script = """@echo off

set ERL_LIBS=%cd%\\{erl_libs_path}

echo on
"{erlang_home}\\bin\\erl" {extra_erl_args} %* || exit /b 1
""".format(
            erlang_home = windows_path(erlang_home),
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
        "erlang_installation": attr.label(
            mandatory = True,
            providers = [ErlangInstallationInfo],
        ),
        "is_windows": attr.bool(mandatory = True),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "extra_erl_args": attr.string_list(),
    },
    executable = True,
)
