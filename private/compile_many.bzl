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

    compiler_flags = {}

    outputs = []
    for app in ctx.attr.apps:
        erlc_opts = app[ErlcOptsInfo]
        source_info = app[ErlangAppSourcesInfo]
        compiler_flags[source_info.app_name] = erlc_opts
        for src in source_info.srcs:
            if src.basename.hassuffix(".erl"):
                out = ctx.actions.declare_file(path_join(
                    ctx.label.name,
                    source_info.app_name,
                    "ebin",
                    src.basename.removesuffix(".erl") + ".beam",
                ))
            else:
                rp = additional_file_dest_relative_path(app, src)
                out = ctx.actions.declare_file(path_join(
                    ctx.label.name,
                    source_info.app_name,
                    rp,
                ))
            outputs.append(out)
            print(out.short_path)

    targets_file = ctx.actions.declare_file()
    ctx.actions.write(
        output = targets_file,
        content = json.encode(compiler_flags),
    )

    args = ctx.actions.args()
    args.add(targets_file)
    args.add_all([
        app[ErlangAppSourcesInfo].srcs[0]
        for app in ctx.attr.apps
    ])

    erl_libs_dirs = [
        path_join(
            ctx.bin_dir.path,
            eld.label.workspace_root,
            eld.label.package,
            eld.label.name,
        )
        for eld in ctx.attr.erl_libs
    ]

    ctx.actions.run(
        inputs = ctx.files.erl_libs + ctx.files.apps + [targets_file],
        outputs = outputs,
        executable = ctx.executable.rules_erlang_compiler,
        arguments = [args],
        mnemonic = "RulesErlangErlc",
        env = {
            "ERL_LIBS": ctx.host_path_separator.join(erl_libs_dirs),
        },
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
        # "erlc_opts": attr.label(
        #     providers = [ErlcOptsInfo],
        # ),
    },
    provides = [CompileManyInfo],
    # toolchains = ["//tools:toolchain_type"],
)
