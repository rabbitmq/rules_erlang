load(":compile_many.bzl", "CompileManyInfo")
load("//:util.bzl", "path_join")

def _impl(ctx):
    # erl_libs = []
    # for eld in ctx.attr.erl_libs:
    #     erl_libs.extend([
    #         path_join(
    #             ctx.bin_dir.path,
    #             eld.label.workspace_root,
    #             eld.label.package,
    #             eld.label.name,
    #             app,
    #             "ebin",
    #         )
    #         for app in eld[CompileManyInfo].apps.keys()
    #     ])
    erl_libs = [
        path_join(
            ctx.bin_dir.path,
            eld.label.workspace_root,
            eld.label.package,
            eld.label.name,
        )
        for eld in ctx.attr.erl_libs
    ]

    args = ctx.actions.args()
    args.add(ctx.outputs.out)
    args.add_all(ctx.attr.apps)

    extract_many_tool_runfiles = ctx.attr.extract_many_tool[DefaultInfo].default_runfiles

    ctx.actions.run(
        inputs = (extract_many_tool_runfiles.files.to_list() +
                  ctx.files.erl_libs),
        outputs = [ctx.outputs.out],
        executable = ctx.executable.extract_many_tool,
        mnemonic = "RulesErlangExtractManyTransitive",
        arguments = [args],
        env = {
            "ERL_LIBS": ctx.configuration.host_path_separator.join(erl_libs),
        },
    )

    return [DefaultInfo(
        files = depset([ctx.outputs.out]),
    )]

extract_many_transitive = rule(
    implementation = _impl,
    attrs = {
        "apps": attr.string_list(),
        "erl_libs": attr.label_list(
            providers = [CompileManyInfo],
        ),
        "out": attr.output(),
        "extract_many_tool": attr.label(
            mandatory = True,
            executable = True,
            cfg = "target",
        ),
    },
)
