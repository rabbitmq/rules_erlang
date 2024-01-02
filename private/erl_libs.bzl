load("//:erlang_app_info.bzl", "ErlangAppInfo")
load("//:util.bzl", "path_join")
load(":compile_many.bzl", "CompileManyInfo")
load(":erlang_app_sources.bzl", "ErlangAppSourcesInfo")
load(":util.bzl", "additional_file_dest_relative_path", "copy")

def _module_name(f):
    return f.basename.removesuffix(".erl")

def _impl(ctx):
    apps = {}
    module_index = {}

    outputs = []
    for app in ctx.attr.apps:
        app_info = app[ErlangAppInfo]

        app_outs = []
        for b in app_info.beam:
            if b.is_directory:
                if len(app_info.beam) != 1:
                    fail("ErlangAppInfo.beam must be a collection of files, or a single ebin dir")
                dest = ctx.actions.declare_directory(path_join(
                    ctx.label.name,
                    app_info.app_name,
                    "ebin",
                ))
                ctx.actions.run_shell(
                    inputs = [b],
                    outputs = [dest],
                    command = "cp -RL \"{}\"/* \"{}\"".format(b.path, dest.path),
                )
            else:
                dest = copy(ctx, b, path_join(
                    ctx.label.name,
                    app_info.app_name,
                    "ebin",
                    b.basename
                ))
                module_index[_module_name(b)] = app_info.app_name
            app_outs.append(dest)

        for hdr in app_info.include:
            rp = additional_file_dest_relative_path(app.label, hdr)
            out = copy(ctx, hdr, path_join(
                ctx.label.name,
                app_info.app_name,
                rp,
            ))
            app_outs.append(out)

        app_src = None
        public_hdrs = []
        private_hdrs = []
        srcs = []
        priv = []
        license_files = []
        for src in app_info.srcs:
            if src.basename.endswith(".app"):
                if src.basename != app_info.app_name + ".app.src":
                    fail("Unexpected .app file", src)
                app_src = src
            elif src.basename.endswith(".hrl"):
                if src.path.find("include/") >= 0:
                    public_hdrs.append(src)
                else:
                    private_hdrs.append(src)
            elif src.basename.endswith(".erl"):
                srcs.append(src)
            elif src.basename.startswith("LICENSE"):
                license_files.append(src)
            elif src.path.find("priv/") >= 0:
                priv.append(src)
            # else:
            #     print("Dropping unrecognized file", src)

        reconstructed_source_info = ErlangAppSourcesInfo(
            app_name = app_info.app_name,
            erlc_opts_file = None,
            app_src = app_src, # single file
            public_hdrs = public_hdrs,
            private_hdrs = private_hdrs,
            srcs = srcs,
            priv = priv,
            license_files = license_files,
        )

        apps[app_info.app_name] = struct(
            source_info = reconstructed_source_info,
            outs = app_outs,
        )
        outputs.extend(app_outs)


    return [
        CompileManyInfo(
            module_index = module_index,
            apps = apps,
        ),
        DefaultInfo(
            files = depset(outputs),
        ),
    ]

erl_libs = rule(
    implementation = _impl,
    attrs = {
        "apps": attr.label_list(
            mandatory = True,
            providers = [ErlangAppInfo],
        )
    },
    provides = [CompileManyInfo],
)
