load("//:util.bzl", "path_join")
load(":compile_many.bzl", "CompileManyInfo")

def _impl(ctx):
    apps = ctx.attr.erl_libs[CompileManyInfo].apps

    if ctx.attr.app_name not in apps:
        fail("requested app not present in erl_libs")

    app_info = apps[ctx.attr.app_name]

    outputs = []
    for src in app_info.source_info.srcs:
        if not src.basename.endswith(".erl"):
            outputs.append(src)
    for out in app_info.outs:
        if out.basename.endswith(".beam"):
            dest = ctx.actions.declare_file(path_join(
                ctx.attr.beam_dest,
                out.basename,
            ))
            ctx.actions.symlink(
                output = dest,
                target_file = out,
            )
            outputs.append(dest)

    return [
        DefaultInfo(files = depset(outputs)),
    ]

extract_app = rule(
    implementation = _impl,
    attrs = {
        "erl_libs": attr.label(
            mandatory = True,
            providers = [CompileManyInfo],
        ),
        "app_name": attr.string(
            mandatory = True,
        ),
        "beam_dest": attr.string(
            default = "ebin",
        ),
    },
)
