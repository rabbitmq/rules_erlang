load(":util.bzl", "path_join")

def _impl(ctx):
    outs = [ctx.actions.declare_file(f) for f in ctx.attr.outs]

    args = ctx.actions.args()
    args.add('-noinput')
    args.add('-boot', 'no_dot_erlang')
    args.add_all('-srcs', ctx.attr.srcs)
    args.add_all('-outs', ctx.attr.outs)
    args.add('-eval', ctx.attr.expression)
    ctx.actions.run(
        outputs = outs,
        executable = 'erl',
        arguments = [args],
        mnemonic = 'ERL',
        use_default_shell_env=True,
    )

    return [
        DefaultInfo(files = depset(outs)),
    ]

erl_eval = rule(
    implementation = _impl,
    attrs = {
        # TODO LRB "srcs": attr.label_list(allow_files = True),
        "srcs": attr.string_list(),
        "outs": attr.string_list(),
        "expression": attr.string(
            mandatory = True,
        ),
    },
)
