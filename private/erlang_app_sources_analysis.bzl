load(":erlang_bytecode.bzl", "unique_dirnames")
load(":util.bzl", "additional_file_dest_relative_path")

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
        "analysis_id": "part of the filename for analysis files",
        "analysis": "analysis files for sources",
    },
)

def _impl(ctx):
    analysis_id = ctx.label.name

    hdrs_dirs = unique_dirnames(ctx.files.public_hdrs + ctx.files.private_hdrs)

    worker_runfiles = ctx.attr.erl_attrs_to_json_worker[DefaultInfo].default_runfiles
    common_inputs = (worker_runfiles.files.to_list() +
                     ctx.files.erlc_opts_file +
                     ctx.files.public_hdrs +
                     ctx.files.private_hdrs)

    outputs = []

    for src in ctx.files.srcs:
        rp = additional_file_dest_relative_path(ctx.label, src)
        outpath = rp.removesuffix(".erl") + "." + analysis_id + ".json"

        out = ctx.actions.declare_file(outpath)

        args = [
            src.path,
            out.path,
            "--erlc_opts", ctx.file.erlc_opts_file.path,
        ]
        for d in hdrs_dirs:
            args.extend(["-I", d])

        args_file = ctx.actions.declare_file(outpath + ".args_file")
        ctx.actions.write(
            output = args_file,
            content = "\n".join(args),
        )

        ctx.actions.run(
            inputs = common_inputs + [src, args_file],
            outputs = [out],
            executable = ctx.executable.erl_attrs_to_json_worker,
            mnemonic = "EPP",
            execution_requirements = {
                "supports-workers": "1",
                "requires-worker-protocol": "json",
            },
            arguments = ["@%s" % args_file.path],
        )

        outputs.append(out)

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
            analysis_id = analysis_id,
            analysis = outputs,
        ),
        DefaultInfo(
            files = depset(outputs),
        ),
    ]

erlang_app_sources_analysis = rule(
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
        "erl_attrs_to_json_worker": attr.label(
            mandatory = True,
            executable = True,
            cfg = "exec",
        ),
    },
    provides = [ErlangAppSourcesInfo],
)
