load(
    "//private:erlang_build.bzl",
    "OtpInfo",
)

def _impl(ctx):
    otpinfo = ctx.attr.otp[OtpInfo]
    vars = {
        "OTP_VERSION": otpinfo.version,
        "ERLANG_HOME": otpinfo.erlang_home,
    }
    if otpinfo.release_dir_tar != None:
        vars["ERLANG_RELEASE_TAR_PATH"] = otpinfo.release_dir_tar.path
        vars["ERLANG_RELEASE_TAR_SHORT_PATH"] = otpinfo.release_dir_tar.short_path
        vars["ERLANG_INSTALL_PATH"] = otpinfo.install_path
    return [
        platform_common.ToolchainInfo(otpinfo = otpinfo),
        platform_common.TemplateVariableInfo(vars),
    ]

erlang_toolchain = rule(
    implementation = _impl,
    attrs = {
        "otp": attr.label(
            mandatory = True,
            providers = [OtpInfo],
        ),
    },
    provides = [
        platform_common.ToolchainInfo,
        platform_common.TemplateVariableInfo,
    ],
)

def _build_info(ctx):
    return ctx.toolchains["//tools:toolchain_type"].otpinfo

def erlang_dirs(ctx, short_path = False):
    """Returns (erlang_home, release_dir_tar, runfiles) for the Erlang toolchain.
    
    Args:
        ctx: The rule context
        short_path: If True, return short_path for release_dir_tar (for runfiles/tests).
                   If False (default), return full path (for build actions).
    
    Returns:
        Tuple of (erlang_home, release_dir_tar, runfiles).
        erlang_home is always the fixed install_path for internal/prebuilt erlang.
    """
    info = _build_info(ctx)
    if info.release_dir_tar != None:
        runfiles = ctx.runfiles([
            info.release_dir_tar,
            info.version_file,
        ])
        # Always use the fixed install_path - this ensures path consistency
        # across all sandboxed actions
        erlang_home = info.erlang_home
    else:
        runfiles = ctx.runfiles([
            info.version_file,
        ])
        # For external erlang, use the absolute path
        erlang_home = info.erlang_home
    return (erlang_home, info.release_dir_tar, runfiles)

def maybe_install_erlang(ctx, short_path = False):
    """Returns shell commands to extract Erlang to the fixed install path.
    
    For internal/prebuilt Erlang, this extracts the tar to install_path.
    The mkdir is used as a lock to ensure only one process extracts.
    """
    info = _build_info(ctx)
    release_dir_tar = info.release_dir_tar
    if release_dir_tar == None:
        return ""
    else:
        return """\
mkdir -p $(dirname "{install_path}")
if mkdir "{install_path}" 2>/dev/null; then
    tar --extract \\
        --directory "{install_path}" \\
        --file {release_tar}
fi
export ROOTDIR="{install_path}"\
""".format(
            release_tar = release_dir_tar.short_path if short_path else release_dir_tar.path,
            install_path = info.install_path,
        )

def version_file(ctx):
    info = _build_info(ctx)
    return info.version_file
