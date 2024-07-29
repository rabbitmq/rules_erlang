ErlcOptsInfo = provider(
    doc = "Reusable set of erlc options",
    fields = {
        "values": "Strings to be passed as additional options to erlc",
    },
)

def _impl(ctx):
    args = ctx.actions.args()
    args.add_all(ctx.attr.values)

    ctx.actions.write(
        output = ctx.outputs.out,
        content = args,
    )

    return [
        ErlcOptsInfo(values = ctx.attr.values),
    ]

erlc_opts_file = rule(
    implementation = _impl,
    attrs = {
        "values": attr.string_list(),
        "out": attr.output(
            mandatory = True,
        ),
    },
    provides = [ErlcOptsInfo],
)
