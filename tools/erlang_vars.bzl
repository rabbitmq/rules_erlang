def _impl(ctx):
    otpinfo = ctx.toolchains["//tools:toolchain_type"].otpinfo
    vars = {
        "OTP_VERSION": otpinfo.version,
        "ERLANG_HOME": otpinfo.erlang_home,
    }
    if otpinfo.release_dir_tar != None:
        vars["ERLANG_RELEASE_TAR_PATH"] = otpinfo.release_dir_tar.path
        vars["ERLANG_RELEASE_TAR_SHORT_PATH"] = otpinfo.release_dir_tar.short_path

    return [platform_common.TemplateVariableInfo(vars)]

erlang_vars = rule(
    implementation = _impl,
    provides = [
        platform_common.TemplateVariableInfo,
    ],
    toolchains = ["//tools:toolchain_type"],
)
