load("//:erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load("//:erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load("//:util.bzl", "path_join")

def beam_file(ctx, src, dir):
    name = src.basename.replace(".erl", ".beam")
    return ctx.actions.declare_file(path_join(dir, name))

def _unique_dirnames(files):
    dirs = []
    for f in files:
        dirname = f.path if f.is_directory else f.dirname
        if dirname not in dirs:
            dirs.append(dirname)
    return dirs

def _impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    beam_files = [beam_file(ctx, src, ctx.attr.dest) for src in ctx.files.srcs]

    dest_dir = beam_files[0].dirname

    erl_args = ctx.actions.args()
    erl_args.add("-v")

    for dir in _unique_dirnames(ctx.files.hdrs):
        erl_args.add("-I", dir)

    for dep in ctx.attr.deps:
        lib_info = dep[ErlangAppInfo]
        if lib_info.erlang_version != erlang_version:
            fail("Mismatched erlang versions", erlang_version, lib_info.erlang_version)
        for dir in _unique_dirnames(lib_info.include):
            erl_args.add("-I", path_join(dir, "../.."))
        for dir in _unique_dirnames(lib_info.beam):
            erl_args.add("-pa", dir)

    for dir in _unique_dirnames(ctx.files.beam):
        erl_args.add("-pa", dir)

    erl_args.add("-o", dest_dir)

    erl_args.add_all(ctx.attr.erlc_opts)

    erl_args.add_all(ctx.files.srcs)

    inputs = []
    inputs.extend(ctx.files.hdrs)
    inputs.extend(ctx.files.srcs)
    for dep in ctx.attr.deps:
        lib_info = dep[ErlangAppInfo]
        inputs.extend(lib_info.include)
        inputs.extend(lib_info.beam)
    inputs.extend(ctx.files.beam)

    ctx.actions.run(
        outputs = beam_files,
        inputs = inputs,
        executable = 'erlc',
        arguments = [erl_args],
        mnemonic = 'ERLC',
        use_default_shell_env=True,
    )

    return [
        DefaultInfo(files = depset(beam_files)),
    ]

erlc_private = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
        "is_windows": attr.bool(mandatory = True),
        "hdrs": attr.label_list(allow_files = [".hrl"]),
        "srcs": attr.label_list(
            mandatory = True,
            allow_files = [".erl"],
        ),
        "beam": attr.label_list(allow_files = [".beam"]),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "erlc_opts": attr.string_list(),
        "dest": attr.string(
            default = "ebin",
        ),
    },
)
