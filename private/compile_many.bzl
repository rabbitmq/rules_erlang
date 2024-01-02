load("//:util.bzl", "path_join")
load(":erlang_app_sources.bzl", "ErlangAppSourcesInfo")
load(":util.bzl", "additional_file_dest_relative_path")

CompileManyInfo = provider(
    doc = "An ERL_LIBS dir layout of compiled erlang apps",
    fields = {
        "module_index": "A map of modulename to application name",
        "apps": "A map of applications to outputs",
    },
)

def _impl(ctx):
    apps = {}

    compiler_flags = struct(
        label = str(ctx.label),
        module_index = {},
        code_paths = [],
        targets = {},
    )

    ## Note: if we write the analysis info out to additional files,
    ##       we could feed those into a rule that produced a deferred
    ##       compilation script that could be used by source debs

    outputs = []
    apps_inputs = []
    for app in ctx.attr.apps:
        source_info = app[ErlangAppSourcesInfo]
        compiler_srcs = (source_info.public_hdrs +
                         source_info.private_hdrs +
                         source_info.srcs)
        compiler_flags.targets[source_info.app_name] = {
            "src_path": path_join(app.label.workspace_root, app.label.package),
            "erlc_opts_file": source_info.erlc_opts_file.path,
            "app_src": source_info.app_src.path,
            "srcs": [s.path for s in compiler_srcs],
        }

        apps_inputs.extend(source_info.public_hdrs)
        apps_inputs.extend(source_info.private_hdrs)
        apps_inputs.extend(source_info.srcs)
        apps_inputs.append(source_info.app_src)
        apps_inputs.append(source_info.erlc_opts_file)

        app_outs = []
        for src in source_info.public_hdrs + source_info.private_hdrs + source_info.srcs:
            if src.basename.endswith(".erl"):
                module_name = src.basename.removesuffix(".erl")
                out = ctx.actions.declare_file(path_join(
                    ctx.label.name,
                    source_info.app_name,
                    "ebin",
                    module_name + ".beam",
                ))
                app_outs.append(out)
                compiler_flags.module_index[module_name] = source_info.app_name
            rp = additional_file_dest_relative_path(app.label, src)
            out = ctx.actions.declare_file(path_join(
                ctx.label.name,
                source_info.app_name,
                rp,
            ))
            app_outs.append(out)
        dot_app = ctx.actions.declare_file(path_join(
            ctx.label.name,
            source_info.app_name,
            "ebin",
            source_info.app_name + ".app",
        ))
        app_outs.append(dot_app)
        compiler_flags.targets[source_info.app_name]["outs"] = [
            o.path
            for o in app_outs
        ]
        apps[source_info.app_name] = struct(
            source_info = source_info,
            outs = app_outs,
        )
        outputs.extend(app_outs)

    for eld in ctx.attr.erl_libs:
        compiler_flags.code_paths.extend([
            path_join(
                ctx.bin_dir.path,
                eld.label.workspace_root,
                eld.label.package,
                eld.label.name,
                app,
                "ebin",
            )
            for app in eld[CompileManyInfo].apps.keys()
        ])

    targets_file = ctx.actions.declare_file(ctx.label.name + "_targets.json")
    ctx.actions.write(
        output = targets_file,
        content = json.encode(compiler_flags),
    )

    args_file = ctx.actions.declare_file(ctx.label.name + ".args_file")
    ctx.actions.write(
        output = args_file,
        content = "\n".join([
            targets_file.path,
        ]),
    )

    compiler_runfiles = ctx.attr.rules_erlang_compiler[DefaultInfo].default_runfiles

    inputs = (compiler_runfiles.files.to_list() +
              ctx.files.erl_libs +
              apps_inputs +
              [targets_file, args_file])

    ctx.actions.run(
        inputs = inputs,
        outputs = outputs,
        executable = ctx.executable.rules_erlang_compiler,
        mnemonic = "RulesErlangErlc",
        execution_requirements = {
            "supports-workers": "1",
            "requires-worker-protocol": "json",
        },
        arguments = ["@%s" % args_file.path],
    )

    return [
        CompileManyInfo(
            module_index = compiler_flags.module_index,
            apps = apps,
        ),
        DefaultInfo(
            files = depset(outputs),
        ),
    ]

compile_many = rule(
    implementation = _impl,
    attrs = {
        "apps": attr.label_list(
            mandatory = True,
            providers = [ErlangAppSourcesInfo],
        ),
        "erl_libs": attr.label_list(
            providers = [CompileManyInfo],
        ),
        "rules_erlang_compiler": attr.label(
            mandatory = True,
            executable = True,
            cfg = "target",
        ),
    },
    provides = [CompileManyInfo],
)
