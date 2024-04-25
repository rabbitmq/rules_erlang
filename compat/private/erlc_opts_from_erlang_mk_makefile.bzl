def _impl(ctx):
    script = """set -euo pipefail

ABS_EXTRACT="$PWD/{extract}"
ABS_OUT="$PWD/{out}"

cd $(dirname "{makefile}")
{gmake} -f Makefile -f $ABS_EXTRACT erlc_opts_file {make_vars}
mv erlc_opts_file $ABS_OUT
"""

    makefile = None
    for src in ctx.files.srcs:
        if src.basename == "Makefile":
            makefile = src
            break

    if makefile == None:
        fail("Makefile not found in {}".format(ctx.attr.srcs))

    gmake_path = ctx.toolchains["//gmake:toolchain_type"].gmake_path

    ctx.actions.run_shell(
        inputs = ctx.files.srcs + ctx.files.extract_mk,
        outputs = [ctx.outputs.out],
        command = script.format(
            gmake = gmake_path,
            makefile = makefile.path,
            extract = ctx.file.extract_mk.path,
            out = ctx.outputs.out.path,
            make_vars = " ".join([
                '{}="{}"'.format(k, v)
                for (k, v) in ctx.attr.make_vars.items()
            ]),
        ),
        mnemonic = "ErlcOptsFromErlangMkMakefile",
    )

erlc_opts_from_erlang_mk_makefile = rule(
    implementation = _impl,
    attrs = {
        "srcs": attr.label_list(
            mandatory = True,
            allow_files = True,
        ),
        "make_vars": attr.string_dict(),
        "out": attr.output(),
        "extract_mk": attr.label(
            default = Label("extract_erlc_opts.mk"),
            allow_single_file = True,
        ),
    },
    toolchains = ["//gmake:toolchain_type"],
)
