load("//:erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load("//:erlang_app_info.bzl", "ErlangAppInfo")
load(
    "//:util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
    "path_join",
)
load(":util.bzl", "erl_libs_contents")

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

    erl_libs_dir = ctx.label.name + "_deps"

    erl_libs_files = erl_libs_contents(
        ctx,
        transitive = False,
        headers = True,
        dir = erl_libs_dir,
    )

    # it would be nice to properly compute the path, but ctx.bin_dir.path
    # does not appear to be the whole prefix
    if len(erl_libs_files) > 0:
        (output_dir, _, path) = erl_libs_files[0].path.partition(erl_libs_dir)
        if output_dir == "":
            fail("Could not compute the ERL_LIBS relative path from {}".format(
                erl_libs_files[0].path,
            ))
        erl_libs_path = path_join(output_dir.rstrip("/"), erl_libs_dir)
    else:
        erl_libs_path = ""

    beam_files = [beam_file(ctx, src, ctx.attr.dest) for src in ctx.files.srcs]

    dest_dir = beam_files[0].dirname

    include_args = []
    for dir in unique_dirnames(ctx.files.hdrs):
        include_args.extend(["-I", dir])

    pa_args = []
    for dir in unique_dirnames(ctx.files.beam):
        pa_args.extend(["-pa", dir])

    srcs = ctx.actions.args()
    srcs.add_all(ctx.files.srcs)

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

        if [ -n "{erl_libs_path}" ]; then
            export ERL_LIBS={erl_libs_path}
        fi

        # use the compile_first escript to determine the first pass, if present
        if [ -n "{compile_first}" ]; then
            FIRST=$("{erlang_home}"/bin/escript {compile_first} $@)
        else
            FIRST=
        fi

        if [ -n "$FIRST" ]; then
            "{erlang_home}"/bin/erlc \\
                -v {include_args} {pa_args} -o {out_dir} {erlc_opts} \\
                $FIRST
            "{erlang_home}"/bin/erlc \\
                -v {include_args} {pa_args} -pa {out_dir} -o {out_dir} {erlc_opts} \\
                $@
        else
            "{erlang_home}"/bin/erlc \\
                -v {include_args} {pa_args} -o {out_dir} {erlc_opts} \\
                $@
        fi
    """.format(
        dest_dir = dest_dir,
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        erlang_version = erlang_version,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
        erl_libs_path = erl_libs_path,
        compile_first = ctx.file.compile_first.path if ctx.file.compile_first != None else "",
        include_args = " ".join(include_args),
        pa_args = " ".join(pa_args),
        out_dir = dest_dir,
        erlc_opts = " ".join(ctx.attr.erlc_opts),
    )

    inputs = []
    inputs.extend(ctx.files.hdrs)
    inputs.extend(ctx.files.srcs)
    inputs.extend(ctx.files.beam)
    inputs.extend(erl_libs_files)
    if ctx.file.compile_first != None:
        inputs.append(ctx.file.compile_first)

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = beam_files,
        command = script,
        arguments = [srcs],
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
        "compile_first": attr.label(
            allow_single_file = True,
        ),
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
