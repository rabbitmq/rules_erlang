load("//:erlang_bytecode2.bzl", "ErlcOptsInfo")
load("//:util.bzl", "path_join")
load(":erlang_app_sources.bzl", "ErlangAppSourcesInfo")
load(":util.bzl", "additional_file_dest_relative_path")

CompileManyInfo = provider(
    doc = "An ERL_LIBS dir layout of compiled erlang apps",
    fields = {},
)

def _impl(ctx):
    # can we figure out all the output files?
    # I guess probably yes

    compiler_flags = {
        "dest_dir": path_join(
            ctx.bin_dir.path,
            ctx.label.package,
            ctx.label.name,
        ),
        "targets": {},
    }

    outputs = []
    for app in ctx.attr.apps:
        erlc_opts = app[ErlcOptsInfo]
        source_info = app[ErlangAppSourcesInfo]
        compiler_flags["targets"][source_info.app_name] = {
            "path": path_join(app.label.workspace_root, app.label.package),
            "erlc_opts": erlc_opts.values,
            "srcs": [s.path for s in source_info.srcs],
            "analysis": [a.path for a in source_info.analysis],
        }
        app_outs = []
        for src in source_info.srcs:
            if src.basename.endswith(".erl"):
                out = ctx.actions.declare_file(path_join(
                    ctx.label.name,
                    source_info.app_name,
                    "ebin",
                    src.basename.removesuffix(".erl") + ".beam",
                ))
                app_outs.append(out)
            rp = additional_file_dest_relative_path(app.label, src)
            out = ctx.actions.declare_file(path_join(
                ctx.label.name,
                source_info.app_name,
                rp,
            ))
            app_outs.append(out)
        compiler_flags["targets"][source_info.app_name]["outs"] = [
            o.path
            for o in app_outs
        ]
        outputs.extend(app_outs)

    targets_file = ctx.actions.declare_file(ctx.label.name + "_targets.json")
    ctx.actions.write(
        output = targets_file,
        content = json.encode(compiler_flags),
    )

    args = ctx.actions.args()
    args.add(targets_file)

    erl_libs_dirs = [
        path_join(
            ctx.bin_dir.path,
            eld.label.workspace_root,
            eld.label.package,
            eld.label.name,
        )
        for eld in ctx.attr.erl_libs
    ]

    compiler_runfiles = ctx.attr.rules_erlang_compiler[DefaultInfo].default_runfiles

    env = {}
    if len(erl_libs_dirs) > 0:
        env["ERL_LIBS"] = ctx.configuration.host_path_separator.join(erl_libs_dirs)

    ctx.actions.run(
        inputs = compiler_runfiles.files.to_list() + ctx.files.erl_libs + ctx.files.apps + [targets_file],
        outputs = outputs,
        executable = ctx.executable.rules_erlang_compiler,
        arguments = [args],
        mnemonic = "RulesErlangErlc",
        env = env,
    )

    return [
        CompileManyInfo(),
        DefaultInfo(files = depset(outputs)),
    ]

compile_many = rule(
    implementation = _impl,
    attrs = {
        "apps": attr.label_list(
            mandatory = True,
            providers = [ErlcOptsInfo, ErlangAppSourcesInfo],
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
    # toolchains = ["//tools:toolchain_type"],
)
