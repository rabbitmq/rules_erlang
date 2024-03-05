def _impl(ctx):
    args = ctx.actions.args()
    args.add(ctx.file.src)
    args.add(ctx.outputs.out)

    extractor_runfiles = ctx.attr.extractor[DefaultInfo].default_runfiles

    ctx.actions.run(
        inputs = (extractor_runfiles.files.to_list()
                     + ctx.files.src),
        outputs = [ctx.outputs.out],
        executable = ctx.executable.extractor,
        mnemonic = "RulesErlangRebarConfigToErlcOpts",
        arguments = [args],
    )

    return [DefaultInfo(
        files = depset([ctx.outputs.out]),
    )]

erlc_opts_from_rebar_config = rule(
    implementation = _impl,
    attrs = {
        "src": attr.label(
            mandatory = True,
            allow_single_file = ["rebar.config"],
        ),
        "out": attr.output(),
        "extractor": attr.label(
            mandatory = True,
            executable = True,
            cfg = "exec",
        ),
    }
)
