load("//:util.bzl", "path_join")

def _erlang_headers_impl(ctx):
    commands = ["set -euo pipefail", ""]

    otpinfo = ctx.toolchains["//tools:toolchain_type"].otpinfo

    if otpinfo.release_dir != None:
        otp_root = otpinfo.release_dir.path
        inputs = [otpinfo.release_dir]
    else:
        otp_root = otpinfo.erlang_home
        inputs = []
    inputs.append(otpinfo.version_file)

    outs = []
    for f in ctx.attr.filenames:
        dest = ctx.actions.declare_file(path_join(ctx.label.name, f))
        commands.append("cp {otp}/lib/erlang/usr/include/{f} {dest}".format(
            otp = otp_root,
            f = f,
            dest = dest.path,
        ))
        outs.append(dest)

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = outs,
        command = "\n".join(commands),
    )

    return [DefaultInfo(files = depset(outs))]

erlang_headers = rule(
    implementation = _erlang_headers_impl,
    attrs = {
        "filenames": attr.string_list(
            default = [
                "driver_int.h",
                "ei.h",
                "ei_connect.h",
                "eicode.h",
                "erl_driver.h",
                "erl_drv_nif.h",
                "erl_fixed_size_int_types.h",
                "erl_int_sizes_config.h",
                "erl_memory_trace_parser.h",
                "erl_nif.h",
                "erl_nif_api_funcs.h",
            ],
        ),
    },
    toolchains = [":toolchain_type"],
)
