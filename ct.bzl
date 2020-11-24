def _impl(ctx):
    script = """
exec env HOME=${TEST_TMPDIR} ct_run \\
    -no_auto_compile \\
    -noinput \\
    -pa src test \\
    -dir test \\
    -logdir ${TEST_UNDECLARED_OUTPUTS_DIR}
    """

    ctx.actions.write(
        output = ctx.outputs.executable,
        content = script,
    )

    suites = ctx.runfiles(files = ctx.files.compiled_suites)
    srcs = ctx.runfiles(files = ctx.files.compiled_srcs)
    return [DefaultInfo(runfiles = suites.merge(srcs))]

ct_test = rule(
    implementation = _impl,
    attrs = {
        "compiled_suites": attr.label_list(allow_files=[".beam"]),
        "compiled_srcs": attr.label_list(allow_files=[".beam"]),
    },
    test = True,
)