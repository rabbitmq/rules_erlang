def _impl(ctx):
    header = ctx.actions.declare_file("header")
    ctx.actions.write(
        output = header,
        content = """#!/usr/bin/env escript
%% This is an -*- erlang -*- file
%%! -escript main {NAME}
""".format(NAME=ctx.label.name)
    )

    escript_zip = ctx.actions.declare_file("escript.zip")
    zipper_inputs = []
    zipper_args = ctx.actions.args()
    zipper_args.add("c", escript_zip)
    zipper_args.add_all(
        ["{dst}={src}".format(dst=f.basename, src=f.path) for f in ctx.files.compiled_srcs]
    )
    ctx.actions.run(
        inputs = ctx.files.compiled_srcs,
        outputs = [escript_zip],
        executable = ctx.executable._zipper,
        arguments = [zipper_args],
        progress_message = "Zipping erlang files...",
        mnemonic = "zipper",
    )

    ctx.actions.run_shell(
        inputs = [header, escript_zip],
        outputs = [ctx.outputs.executable],
        command = "cat {h} {t} >> {out} && chmod +x {out}".format(h=header.path, t=escript_zip.path, out=ctx.outputs.executable.path),
    )

escript = rule(
    implementation = _impl,
    attrs = {
        "compiled_srcs": attr.label_list(allow_files=[".beam"]),
        "_zipper": attr.label(default = Label("@bazel_tools//tools/zip:zipper"), cfg = "host", executable=True),
    },
    executable = True,
)