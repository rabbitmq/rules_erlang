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
    if otpinfo.release_dir != None:
        vars["ERLANG_RELEASE_DIR_PATH"] = otpinfo.release_dir.path
        vars["ERLANG_RELEASE_DIR_SHORT_PATH"] = otpinfo.release_dir.short_path
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
        # Instead of using this toolchain for a genrule,
        # since toolchain resolution won't yet have applied,
        # use @rules_erlang//tools:erlang_vars as a
        # toolchain for genrule rules
        platform_common.TemplateVariableInfo,
    ],
)

def _build_info(ctx):
    return ctx.toolchains["//tools:toolchain_type"].otpinfo

def erlang_dirs(ctx, short_path = False):
    """Returns (erlang_home, release_dir, runfiles) for the Erlang toolchain.
    
    Args:
        ctx: The rule context
        short_path: If True, return short_path for erlang_home (for runfiles/tests).
                   If False (default), return full path (for build actions).
    """
    info = _build_info(ctx)
    if info.release_dir != None:
        runfiles = ctx.runfiles([
            info.release_dir,
            info.version_file,
        ])
        if short_path:
            # For runfiles (tests use $TEST_SRCDIR/$TEST_WORKSPACE, run uses $RUNFILES_DIR)
            # Use a shell expression that works for both cases
            erlang_home = "${TEST_SRCDIR:-$RUNFILES_DIR}/${TEST_WORKSPACE:-_main}/" + info.release_dir.short_path
        else:
            erlang_home = info.release_dir.path
    else:
        runfiles = ctx.runfiles([
            info.version_file,
        ])
        # For external erlang, always use the absolute path
        erlang_home = info.erlang_home
    return (erlang_home, info.release_dir, runfiles)

def maybe_install_erlang(ctx, short_path = False):
    """Returns shell commands to set up the Erlang environment.
    
    For internal/prebuilt Erlang, this sets ROOTDIR to help Erlang find its libraries.
    """
    info = _build_info(ctx)
    if info.release_dir != None:
        if short_path:
            # Set ROOTDIR to tell Erlang where its installation is
            # This prevents Erlang from using resolved symlink paths
            rootdir = "${TEST_SRCDIR:-$RUNFILES_DIR}/${TEST_WORKSPACE:-_main}/" + info.release_dir.short_path
            return 'export ROOTDIR="' + rootdir + '"'
        else:
            return 'export ROOTDIR="' + info.release_dir.path + '"'
    return ""

def version_file(ctx):
    info = _build_info(ctx)
    return info.version_file
