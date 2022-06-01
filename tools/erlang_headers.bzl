load("//:util.bzl", "path_join")
load(
    ":erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_symlink_erlang",
)

def _erlang_headers_impl(ctx):
    commands = [
        "set -euo pipefail",
        "",
        maybe_symlink_erlang(ctx),
        "",
    ]

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    outs = []
    for f in ctx.attr.filenames:
        dest = ctx.actions.declare_file(path_join(ctx.label.name, f))
        commands.append("cp '{erlang_home}'/usr/include/{f} {dest}".format(
            erlang_home = erlang_home,
            f = f,
            dest = dest.path,
        ))
        outs.append(dest)

    ctx.actions.run_shell(
        inputs = runfiles.files,
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
