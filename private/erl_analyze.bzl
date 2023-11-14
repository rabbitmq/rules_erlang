load(":erlang_bytecode.bzl", "unique_dirnames")
load(":util.bzl", "additional_file_dest_relative_path")
load("//:erlang_bytecode2.bzl", "ErlcOptsInfo")

ErlAnalyzeInfo = provider(
    doc = "Produced by the erl_analyze rule",
    fields = {},
)

def _macros(erlc_opts):
    return [opt for opt in erlc_opts if opt.startswith("-D")]

def _impl(ctx):
    macros = _macros(ctx.attr.erlc_opts[ErlcOptsInfo].values)
    hdrs_dirs = unique_dirnames(ctx.files.hdrs)

    outputs = []

    for src in ctx.files.srcs:
        rp = additional_file_dest_relative_path(ctx.label, src)
        outpath = rp.removesuffix(".erl") + "." + ctx.attr.suffix + ".json"

        out = ctx.actions.declare_file(outpath)

        args = [src.path, out.path]
        if ctx.attr.erlc_opts != None:
            args.extend(macros)
        for d in hdrs_dirs:
            args.extend(["-I", d])

        args_file = ctx.actions.declare_file(outpath + ".args_file")
        ctx.actions.write(
            output = args_file,
            content = "\n".join(args),
        )

        worker_runfiles = ctx.attr.erl_attrs_to_json_worker[DefaultInfo].default_runfiles

        ctx.actions.run(
            inputs = worker_runfiles.files.to_list() + ctx.files.hdrs + [src, args_file],
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
        ctx.attr.erlc_opts[ErlcOptsInfo],
        ErlAnalyzeInfo(),
        DefaultInfo(files = depset(outputs)),
    ]

erl_analyze = rule(
    implementation = _impl,
    attrs = {
        "hdrs": attr.label_list(
            allow_files = [".hrl"],
        ),
        "srcs": attr.label_list(
            mandatory = True,
            allow_files = [".erl"],
        ),
        "suffix": attr.string(
            default = "normal",
        ),
        "erlc_opts": attr.label(
            providers = [ErlcOptsInfo],
        ),
        "erl_attrs_to_json_worker": attr.label(
            mandatory = True,
            executable = True,
            cfg = "exec",
        ),
    },
    provides = [ErlcOptsInfo, ErlAnalyzeInfo],
    # toolchains = ["//tools:toolchain_type"],
)
