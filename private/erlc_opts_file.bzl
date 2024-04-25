def _impl(ctx):
    args = ctx.actions.args()
    args.add_all(ctx.attr.values)

    ctx.actions.write(
        output = ctx.outputs.out,
        content = args,
    )

erlc_opts_file = rule(
    implementation = _impl,
    attrs = {
        "values": attr.string_list(),
        "out": attr.output(),
    },
)
