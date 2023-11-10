load(":erlang_bytecode.bzl", "unique_dirnames")
load("//:erlang_bytecode2.bzl", "ErlcOptsInfo")

def _macros(erlc_opts):
    return [opt for opt in erlc_opts if opt.startswith("-D")]

def _impl(ctx):
    args = [ctx.file.src.path, ctx.outputs.out.path]
    if ctx.attr.erlc_opts != None:
        args.extend(_macros(ctx.attr.erlc_opts[ErlcOptsInfo].values))
    for d in unique_dirnames(ctx.files.hdrs):
        args.extend(["-I", d])

    args_file = ctx.actions.declare_file(ctx.label.name + "_args_file")
    ctx.actions.write(
        output = args_file,
        content = "\n".join(args),
    )

    worker_runfiles = ctx.attr.erl_attrs_to_json_worker[DefaultInfo].default_runfiles

    ctx.actions.run(
        inputs = worker_runfiles.files.to_list() + ctx.files.hdrs + ctx.files.src + [args_file],
        outputs = [ctx.outputs.out],
        executable = ctx.executable.erl_attrs_to_json_worker,
        mnemonic = "EPP",
        execution_requirements = {
            "supports-workers": "1",
            "requires-worker-protocol": "json",
        },
        arguments = ["@%s" % args_file.path],
    )

erl_analyze = rule(
    implementation = _impl,
    attrs = {
        "hdrs": attr.label_list(
            allow_files = [".hrl"],
        ),
        "src": attr.label(
            mandatory = True,
            allow_single_file = [".erl"],
        ),
        "out": attr.output(
            mandatory = True,
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
    toolchains = ["//tools:toolchain_type"],
)
