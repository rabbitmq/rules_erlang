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

    apps = {}
    erl_libs_files = []
    for el in ctx.attr.erl_libs:
        cm_info = el[CompileManyInfo]
        for (app, props) in cm_info.apps.items():
            if app in apps:
                print("Warning: duplicate apps in", ctx.attr.erl_libs)
            src_path = None
            if props.source_info.app_src != None:
                src_path = props.source_info.app_src.dirname.removesuffix("/src")
            apps[app] = {
                "src_path": src_path,
                "priv": [f.path for f in props.source_info.priv],
                "outs": [f.path for f in props.outs],
            }
            erl_libs_files.extend(props.outs + props.source_info.priv)

    apps_json = ctx.actions.declare_file(ctx.label.name + "_apps.json")
    ctx.actions.write(
        output = apps_json,
        content = json.encode(apps)
    )

    erl_libs_paths = [
        path_join(
            ctx.bin_dir.path,
            eld.label.workspace_root,
            eld.label.package,
            eld.label.name,
        )
        for eld in ctx.attr.erl_libs
    ]

    args = ctx.actions.args()
    args.add("--apps_json")
    args.add(apps_json)
    args.add("--out")
    args.add(ctx.outputs.out)
    args.add_all(ctx.attr.apps)

    extract_many_tool_runfiles = ctx.attr.extract_many_tool[DefaultInfo].default_runfiles

    ctx.actions.run(
        inputs = (extract_many_tool_runfiles.files.to_list() +
                  [apps_json] +
                  erl_libs_files),
        outputs = [ctx.outputs.out],
        executable = ctx.executable.extract_many_tool,
        mnemonic = "RulesErlangExtractManyTransitive",
        arguments = [args],
        env = {
            "ERL_LIBS": ctx.configuration.host_path_separator.join(erl_libs_paths),
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
