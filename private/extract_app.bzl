load("//:util.bzl", "path_join")
load("//:erlang_app_info.bzl", "ErlangAppInfo")
load(":compile_many.bzl", "CompileManyInfo")

def _impl(ctx):
    apps = ctx.attr.erl_libs[CompileManyInfo].apps

    if ctx.attr.app_name not in apps:
        fail("requested app not present in erl_libs")

    beam = []

    app_info = apps[ctx.attr.app_name]
    for out in app_info.outs:
        if out.basename.endswith(".beam") or out.basename.endswith(".app"):
            dest = ctx.actions.declare_file(path_join(
                ctx.attr.beam_dest,
                out.basename,
            ))
            ctx.actions.symlink(
                output = dest,
                target_file = out,
            )
            beam.append(dest)

    return [
        ErlangAppInfo(
            app_name = ctx.attr.app_name,
            extra_apps = [],
            include = app_info.source_info.public_hdrs,
            beam = beam,
            priv = app_info.source_info.priv,
            license_files = app_info.source_info.license_files,
            srcs = (app_info.source_info.public_hdrs +
                    app_info.source_info.private_hdrs +
                    app_info.source_info.srcs +
                    [app_info.source_info.app_src] if app_info.source_info.app_src != None else []),
            deps = [],
        ),
        DefaultInfo(
            files = depset(beam + app_info.source_info.priv),
            runfiles = ctx.runfiles(beam + app_info.source_info.priv),
        ),
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
    provides = [ErlangAppInfo],
)
