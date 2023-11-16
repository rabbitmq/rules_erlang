load("//:util.bzl", "path_join")
load("//:erlang_app_info.bzl", "ErlangAppInfo")
load(":compile_many.bzl", "CompileManyInfo")

def _impl(ctx):
    apps = ctx.attr.erl_libs[CompileManyInfo].apps

    if ctx.attr.app_name not in apps:
        fail("requested app not present in erl_libs")

    app_info = apps[ctx.attr.app_name]

    files = struct(
        include = [],
        beam = [],
        priv = [],
        license_files = [],
        srcs = [],
    )

    for src in app_info.source_info.srcs:
        if src.basename.endswith(".hrl"):
            # need to determine if "private" header
            files.include.append(src)
        elif not src.basename.endswith(".erl"):
            files.priv.append(src)
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
            files.beam.append(dest)

    runfiles = ctx.runfiles(files.beam + files.priv)

    return [
        ErlangAppInfo(
            app_name = ctx.attr.app_name,
            extra_apps = [],
            include = files.include,
            beam = files.beam,
            priv = files.priv,
            license_files = files.license_files,
            srcs = app_info.source_info.srcs,
            deps = [],
        ),
        DefaultInfo(
            files = depset(files.beam + files.priv),
            runfiles = runfiles,
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
