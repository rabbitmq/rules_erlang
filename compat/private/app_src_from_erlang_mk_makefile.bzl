def _impl(ctx):
    script = """set -euo pipefail

ABS_EXTRACT="$PWD/{extract}"
ABS_OUT="$PWD/{out}"

cd $(dirname "{makefile}")
${{MAKE:-gmake}} -f Makefile -f $ABS_EXTRACT $ABS_OUT
"""

    makefile = None
    for src in ctx.files.srcs:
        if src.basename == "Makefile":
            makefile = src
            break

    if makefile == None:
        fail("Makefile not found in {}".format(ctx.attr.srcs))

    ctx.actions.run_shell(
        inputs = ctx.files.srcs + ctx.files.extract_mk,
        outputs = [ctx.outputs.out],
        command = script.format(
            makefile = makefile.path,
            extract = ctx.file.extract_mk.path,
            out = ctx.outputs.out.path,
        ),
        mnemonic = "AppSrcFromErlangMkMakefile",
    )

app_src_from_erlang_mk_makefile = rule(
    implementation = _impl,
    attrs = {
        "srcs": attr.label_list(
            mandatory = True,
            allow_files = True,
        ),
        "out": attr.output(),
        "extract_mk": attr.label(
            default = Label("extract_app_src.mk"),
            allow_single_file = True,
        ),
    },
)
