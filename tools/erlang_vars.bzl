def _impl(ctx):
    otpinfo = ctx.toolchains["//tools:toolchain_type"].otpinfo
    vars = {
        "OTP_VERSION": otpinfo.version,
        "ERLANG_HOME": otpinfo.erlang_home,
    }
    if otpinfo.release_dir != None:
        vars["ERLANG_RELEASE_DIR_PATH"] = otpinfo.release_dir.path
        vars["ERLANG_RELEASE_DIR_SHORT_PATH"] = otpinfo.release_dir.short_path

    return [platform_common.TemplateVariableInfo(vars)]

erlang_vars = rule(
    implementation = _impl,
    provides = [
        platform_common.TemplateVariableInfo,
    ],
    toolchains = ["//tools:toolchain_type"],
)
