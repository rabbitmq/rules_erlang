load(":compile_many.bzl", "CompileManyInfo")

def _impl(ctx):
    apps = {}
    erl_libs_files = []
    for el in ctx.attr.erl_libs:
        cm_info = el[CompileManyInfo]
        for (app, props) in cm_info.apps.items():
            if app in apps:
                print("Warning: duplicate apps in", ctx.attr.erl_libs)
            apps[app] = {
                "priv": [f.path for f in props.source_info.priv],
                "outs": [f.path for f in props.outs],
            }
            erl_libs_files.extend(props.outs + props.source_info.priv)

    apps_json = ctx.actions.declare_file(ctx.label.name + "_apps.json")
    ctx.actions.write(
        output = apps_json,
        content = json.encode(apps)
    )

    args = ctx.actions.args()
    args.add("--apps_json")
    args.add(apps_json)
    args.add("--out")
    args.add(ctx.outputs.out)
    args.add("--versioned_dirs")
    args.add(ctx.attr.versioned_dirs)
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
        "versioned_dirs": attr.bool(),
        "extract_many_tool": attr.label(
            mandatory = True,
            executable = True,
            cfg = "target",
        ),
    },
)
