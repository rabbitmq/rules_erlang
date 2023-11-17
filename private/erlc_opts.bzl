ErlcOptsInfo = provider(
    doc = "Reusable set of erlc options",
    fields = {
        "values": "Strings to be passed as additional options to erlc",
    },
)

def _impl(ctx):
    return [
        ErlcOptsInfo(values = ctx.attr.values),
    ]

erlc_opts = rule(
    implementation = _impl,
    attrs = {
        "values": attr.string_list(),
    },
    provides = [ErlcOptsInfo],
)
