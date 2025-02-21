load("//:erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load("//:util.bzl", "path_join")
load(":compile_many.bzl", "CompileManyInfo")

def _impl(ctx):
    apps = ctx.attr.erl_libs[CompileManyInfo].apps

    if ctx.attr.app_name not in apps:
        fail('requested app "{}" not present in erl_libs'.format(ctx.attr.app_name))

    app_info = apps[ctx.attr.app_name]

    beam = []
    test_beam = []
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

    test_modules = {}
    for src in app_info.source_info.test_srcs:
        module_name = src.basename.removesuffix(".erl")
        test_modules[module_name] = True

    for out in app_info.outs:
        if out.basename.endswith(".beam") or out.basename.endswith(".app"):
            module_name = out.basename.removesuffix(".beam")
            is_test_module = module_name in test_modules
            if not ctx.attr.test and is_test_module:
                continue
            dest = ctx.actions.declare_file(path_join(
                "ebin",
                out.basename,
            ))
            ctx.actions.symlink(
                output = dest,
                target_file = out,
            )
            if is_test_module:
                test_beam.append(dest)
            else:
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

    app_root = app_info.source_info.app_dir.short_path + "/"
    privs = []
    for p in app_info.source_info.priv:
        rp = p.short_path.removeprefix(app_root)
        dest = ctx.actions.declare_file(rp)
        ctx.actions.symlink(
            output = dest,
            target_file = p,
        )
        privs.append(dest)

    outs = beam + test_beam + privs
    if ctx.attr.copy_headers:
        outs += public_hdrs + private_hdrs

    return [
        ErlangAppInfo(
            app_name = ctx.attr.app_name,
            extra_apps = ctx.attr.extra_apps,
            include = public_hdrs + private_hdrs,
            beam = beam,
            test_beam = test_beam,
            priv = privs,
            license_files = app_info.source_info.license_files,
            srcs = (app_info.source_info.public_hdrs +
                    app_info.source_info.private_hdrs +
                    app_info.source_info.srcs +
                    [app_info.source_info.app_src] if app_info.source_info.app_src != None else []),
            test_srcs = app_info.source_info.test_srcs if ctx.attr.test else [],
            test_data = app_info.source_info.test_data if ctx.attr.test else [],
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
        "extra_apps": attr.string_list(),
        "deps": attr.label_list(
            providers = [ErlangAppInfo],
        ),
        "copy_headers": attr.bool(
            default = False,
        ),
        "test": attr.bool(
            default = False,
        ),
    },
    provides = [ErlangAppInfo],
)
