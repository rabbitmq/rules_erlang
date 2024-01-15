load("//:erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load("//:util.bzl", "path_join")
load(":compile_many.bzl", "CompileManyInfo")

def _impl(ctx):
    apps = ctx.attr.erl_libs[CompileManyInfo].apps

    if ctx.attr.app_name not in apps:
        fail("requested app not present in erl_libs")

    app_info = apps[ctx.attr.app_name]

    beam = []
    public_hdrs = []
    private_hdrs = []
    if not ctx.attr.copy_headers:
        public_hdrs = app_info.source_info.public_hdrs
        private_hdrs = app_info.source_info.private_hdrs

    out_base = None
    if ctx.attr.copy_headers:
        for out in app_info.outs:
            if out.basename.endswith(".beam") or out.basename.endswith(".app"):
                (base, sep, _) = out.dirname.rpartition("/")
                out_base = base + sep
                break

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
        elif ctx.attr.copy_headers and out.basename.endswith(".hrl"):
            rp = out.path.removeprefix(out_base)
            dest = ctx.actions.declare_file(rp)
            ctx.actions.symlink(
                output = dest,
                target_file = out,
            )
            if rp.startswith("include/"):
                public_hdrs.append(dest)
            else:
                private_hdrs.append(dest)

    outs = beam + app_info.source_info.priv
    if ctx.attr.copy_headers:
        outs += public_hdrs + private_hdrs

    return [
        ErlangAppInfo(
            app_name = ctx.attr.app_name,
            extra_apps = ctx.attr.extra_apps,
            include = public_hdrs + private_hdrs,
            beam = beam,
            priv = app_info.source_info.priv,
            license_files = app_info.source_info.license_files,
            srcs = (app_info.source_info.public_hdrs +
                    app_info.source_info.private_hdrs +
                    app_info.source_info.srcs +
                    [app_info.source_info.app_src] if app_info.source_info.app_src != None else []),
            deps = flat_deps(ctx.attr.deps),
            direct_deps = ctx.attr.deps,
        ),
        DefaultInfo(
            files = depset(outs),
            runfiles = ctx.runfiles(outs),
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
        "extra_apps": attr.string_list(),
        "deps": attr.label_list(
            providers = [ErlangAppInfo],
        ),
        "copy_headers": attr.bool(
            default = False,
        ),
    },
    provides = [ErlangAppInfo],
)
