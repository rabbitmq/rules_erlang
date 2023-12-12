ErlangAppSourcesInfo = provider(
    doc = "Produced by the erlang_app_sources rule",
    fields = {
        "app_name": "Name of the erlang application",
        "erlc_opts_file": "A file containing the erlc options",
        "extra_apps": "Extra applications in the applications key of the .app file",
        "app_src": ".app.src file if available",
        "public_hdrs": "Public header files",
        "private_hdrs": "Private header files",
        "srcs": "Source files",
        # "beam": "Compiled bytecode (.beam) files, or a single ebin directory",
        "priv": "Additional files",
        "license_files": "License files",
        # "precompiled_beam": "Precompiled bytecode",
    },
)

def _impl(ctx):
    return [
        ErlangAppSourcesInfo(
            app_name = ctx.attr.app_name,
            erlc_opts_file = ctx.file.erlc_opts_file,
            app_src = ctx.file.app_src,
            public_hdrs = ctx.files.public_hdrs,
            private_hdrs = ctx.files.private_hdrs,
            srcs = ctx.files.srcs,
            priv = ctx.files.priv,
            license_files = ctx.files.license_files,
            # precompiled_beam = ctx.files.precompiled_beam,
        ),
    ]

erlang_app_sources = rule(
    implementation = _impl,
    attrs = {
        "app_name": attr.string(
            mandatory = True,
        ),
        "app_src": attr.label(
            mandatory = True,
            allow_single_file = [".app.src"],
        ),
        "public_hdrs": attr.label_list(
            allow_files = [".hrl"],
        ),
        "private_hdrs": attr.label_list(
            allow_files = [".hrl"],
        ),
        "srcs": attr.label_list(
            mandatory = True,
            allow_files = [".erl"],
        ),
        "priv": attr.label_list(
            allow_files = True,
        ),
        "license_files": attr.label_list(
            allow_files = True,
        ),
        # "precompiled_beam": attr.label_list(
        #     allow_files = [".beam"],
        # ),
        "erlc_opts_file": attr.label(
            mandatory = True,
            allow_single_file = True,
        ),
    },
    provides = [ErlangAppSourcesInfo],
)
