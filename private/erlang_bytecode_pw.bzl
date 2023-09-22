load("//:erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load("//:util.bzl", "path_join")
load(":erlang_bytecode.bzl", "unique_dirnames")
load(":erlang_bytecode2.bzl", "ErlcOptsInfo")
load(":util.bzl", "erl_libs_contents")

def _impl(ctx):
    if len(ctx.attr.outs) == 0:
        fail("attr outs must not be empty")

    outputs = [
        ctx.actions.declare_file(f.name)
        for f in ctx.attr.outs
    ]

    erl_libs_dir = ctx.label.name + "_deps"

    target = None
    if ctx.attr.app_name != "":
        target = ErlangAppInfo(
            app_name = ctx.attr.app_name,
            include = ctx.files.hdrs,
        )

    package_dir = path_join(
        ctx.label.workspace_root,
        ctx.label.package,
    )

    # this actually just needs to be headers and behaviours, in the context of compiling,
    # though it must include the transitive headers and behaviors. Therefore this file
    # could have it's own optimized version of `erl_libs_contents`
    erl_libs_files = erl_libs_contents(
        ctx,
        target_info = target,
        headers = True,
        dir = erl_libs_dir,
        deps = flat_deps(ctx.attr.deps),
        ez_deps = ctx.files.ez_deps,
    )

    erl_libs_path = ""
    if len(erl_libs_files) > 0:
        erl_libs_path = path_join(
            ctx.bin_dir.path,
            ctx.label.workspace_root,
            ctx.label.package,
            erl_libs_dir,
        )

    out_dirs = unique_dirnames(outputs)
    if len(out_dirs) > 1:
        fail(ctx.attr.outs, "do not share a common parent directory")
    out_dir = out_dirs[0]

    include_args = []
    if package_dir != "":
        include_args.extend(["-I", package_dir])
    for dir in unique_dirnames(ctx.files.hdrs):
        include_args.extend(["-I", dir])
    if erl_libs_path != "":
        include_args.extend(["-I", erl_libs_path])

    pa_args = []
    for dir in unique_dirnames(ctx.files.beam):
        pa_args.extend(["-pa", dir])

    inputs = ctx.files.hdrs + ctx.files.srcs + ctx.files.beam + erl_libs_files

    args = []
    args.append("--PACKAGE_DIR=%s" % package_dir)
    args.append("--ERL_LIBS=%s" % erl_libs_path)
    args.append("-v")
    args.extend(include_args)
    args.extend(pa_args)
    args.extend(["-o", out_dir])
    args.extend(ctx.attr.erlc_opts[ErlcOptsInfo].values)
    args.extend([f.path for f in ctx.files.srcs])

    args_file = ctx.actions.declare_file(ctx.label.name + "_args_file")
    ctx.actions.write(
        output = args_file,
        content = "\n".join(args),
    )
    ctx.actions.run(
        inputs = inputs + [args_file],
        outputs = outputs,
        executable = ctx.executable.erlc_worker,
        mnemonic = "ERLC",
        execution_requirements = {
            "supports-workers": "1",
            "requires-worker-protocol": "proto",
        },
        arguments = ["@%s" % args_file.path],
    )

    return [
        DefaultInfo(files = depset(outputs)),
    ]

erlang_bytecode = rule(
    implementation = _impl,
    attrs = {
        "app_name": attr.string(),
        "hdrs": attr.label_list(
            allow_files = True,
        ),
        "srcs": attr.label_list(
            mandatory = True,
            allow_files = [".erl"],
        ),
        "beam": attr.label_list(
            allow_files = [".beam"],
        ),
        "deps": attr.label_list(
            providers = [ErlangAppInfo],
        ),
        "ez_deps": attr.label_list(
            allow_files = [".ez"],
        ),
        "erlc_opts": attr.label(
            providers = [ErlcOptsInfo],
        ),
        "outs": attr.output_list(
            mandatory = True,
        ),
        "erlc_worker": attr.label(
            mandatory = True,
            executable = True,
            cfg = "exec",
        ),
    },
)
