def _impl(ctx):
    args = [ctx.file.src.path, ctx.outputs.out.path]
    # need to add the macros to the args, at the least, like with gazelle

    args_file = ctx.actions.declare_file(ctx.label.name + "_args_file")
    ctx.actions.write(
        output = args_file,
        content = "\n".join(args),
    )

    ctx.actions.run(
        inputs = ctx.files.src + [args_file],
        outputs = [ctx.outputs.out],
        executable = ctx.executable.erl_attrs_to_json_worker,
        mnemonic = "EPP",
        execution_requirements = {
            "supports-workers": "1",
            "requires-worker-protocol": "json",
        },
        arguments = ["@%s" % args_file.path],
    )

erl_analyze = rule(
    implementation = _impl,
    attrs = {
        "src": attr.label(
            mandatory = True,
            allow_single_file = [".erl"],
        ),
        "out": attr.output(
            mandatory = True,
        ),
        "erl_attrs_to_json_worker": attr.label(
            mandatory = True,
            executable = True,
            cfg = "exec",
        ),
    },
    toolchains = ["//tools:toolchain_type"],
)
