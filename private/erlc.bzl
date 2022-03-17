load("//:erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load("//:erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load(
    "//:util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
    "path_join",
)

def beam_file(ctx, src, dir):
    name = src.basename.replace(".erl", ".beam")
    return ctx.actions.declare_file(path_join(dir, name))

def unique_dirnames(files):
    dirs = []
    for f in files:
        dirname = f.path if f.is_directory else f.dirname
        if dirname not in dirs:
            dirs.append(dirname)
    return dirs

def _dirname(path):
    return path.rpartition("/")[0]

def _impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    beam_files = [beam_file(ctx, src, ctx.attr.dest) for src in ctx.files.srcs]

    dest_dir = beam_files[0].dirname

    erl_args = ctx.actions.args()
    erl_args.add("-v")

    for dir in unique_dirnames(ctx.files.hdrs):
        erl_args.add("-I", dir)

    for dep in ctx.attr.deps:
        lib_info = dep[ErlangAppInfo]
        if lib_info.erlang_version != erlang_version:
            fail("Mismatched erlang versions", erlang_version, lib_info.erlang_version)
        for dir in unique_dirnames(lib_info.include):
            erl_args.add("-I", _dirname(_dirname(dir)))
        for dir in unique_dirnames(lib_info.beam):
            erl_args.add("-pa", dir)

    for dir in unique_dirnames(ctx.files.beam):
        erl_args.add("-pa", dir)

    erl_args.add("-o", dest_dir)

    erl_args.add_all(ctx.attr.erlc_opts)

    erl_args.add_all(ctx.files.srcs)

    script = """
        set -euo pipefail

        mkdir -p {dest_dir}
        export HOME=$PWD

        {begins_with_fun}
        V=$("{erlang_home}"/bin/{query_erlang_version})
        if ! beginswith "{erlang_version}" "$V"; then
            echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
            exit 1
        fi

        "{erlang_home}"/bin/erlc $@
    """.format(
        dest_dir = dest_dir,
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        erlang_version = erlang_version,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
    )

    inputs = []
    inputs.extend(ctx.files.hdrs)
    inputs.extend(ctx.files.srcs)
    for dep in ctx.attr.deps:
        lib_info = dep[ErlangAppInfo]
        inputs.extend(lib_info.include)
        inputs.extend(lib_info.beam)
    inputs.extend(ctx.files.beam)

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = beam_files,
        command = script,
        arguments = [erl_args],
        mnemonic = "ERLC",
    )

    return [
        DefaultInfo(files = depset(beam_files)),
    ]

erlc_private = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
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
