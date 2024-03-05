def _impl(ctx):
    # should we validate the path?

    return [
        platform_common.ToolchainInfo(
            gmake_path = ctx.attr.gmake_path,
        )
    ]

gmake_toolchain = rule(
    implementation = _impl,
    attrs = {
        "gmake_path": attr.string(
            mandatory = True,
        ),
    },
    provides = [platform_common.ToolchainInfo],
)
