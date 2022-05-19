load(
    "//private:erlang_build.bzl",
    "OtpInfo",
)

def _impl(ctx):
    toolchain_info = platform_common.ToolchainInfo(
        otpinfo = ctx.attr.otp[OtpInfo],
    )
    return [toolchain_info]

erlang_toolchain = rule(
    implementation = _impl,
    attrs = {
        "otp": attr.label(
            mandatory = True,
            providers = [OtpInfo],
        ),
    },
    provides = [platform_common.ToolchainInfo],
)

def _build_info(ctx):
    return ctx.toolchains["//tools:toolchain_type"].otpinfo

def erlang_dirs(ctx):
    info = _build_info(ctx)
    if info.release_dir != None:
        runfiles = ctx.runfiles([info.release_dir])
    else:
        runfiles = ctx.runfiles()
    return (info.erlang_home, info.release_dir, runfiles)

def maybe_symlink_erlang(ctx, short_path = False):
    info = _build_info(ctx)
    release_dir = info.release_dir
    if release_dir == None:
        return ""
    else:
        return """mkdir -p $(dirname "{erlang_home}")
ln -sf $PWD/{erlang_release_dir} "{erlang_home}"
mkdir -p "{erlang_home}"/bin
ln -sf ../lib/erlang/bin/ct_run "{erlang_home}"/bin/ct_run
ln -sf ../lib/erlang/bin/dialyzer "{erlang_home}"/bin/dialyzer
ln -sf ../lib/erlang/bin/epmd "{erlang_home}"/bin/epmd
ln -sf ../lib/erlang/bin/erl "{erlang_home}"/bin/erl
ln -sf ../lib/erlang/bin/erlc "{erlang_home}"/bin/erlc
ln -sf ../lib/erlang/bin/escript "{erlang_home}"/bin/escript
ln -sf ../lib/erlang/bin/run_erl "{erlang_home}"/bin/run_erl
ln -sf ../lib/erlang/bin/to_erl "{erlang_home}"/bin/to_erl
ln -sf ../lib/erlang/bin/typer "{erlang_home}"/bin/typer
ERTS_DIRNAME="$(basename "$(echo "{erlang_home}"/lib/erlang/erts-*)")"
ln -sf ../$ERTS_DIRNAME/bin/epmd "{erlang_home}"/lib/erlang/bin/epmd
""".format(
            erlang_release_dir = release_dir.short_path if short_path else release_dir.path,
            erlang_home = info.erlang_home,
        )
