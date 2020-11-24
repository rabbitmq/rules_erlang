def declared_beam_file(ctx, f):
    short_path = f.short_path.replace(".erl", ".beam", 1)
    return ctx.actions.declare_file(short_path)

def _impl(ctx):
    outs = [declared_beam_file(ctx, f) for f in ctx.files.srcs]

    ctx.actions.run_shell(
        inputs = ctx.files.srcs,
        outputs = outs,
        command = "erlc -v -o " + outs[0].dirname + " " + " ".join([f.path for f in ctx.files.srcs]),
        use_default_shell_env = True,
    )

    return [DefaultInfo(files = depset(outs))]

beam = rule(
    implementation = _impl,
    attrs = {
        "srcs": attr.label_list(allow_files=[".erl"]),
    }
)