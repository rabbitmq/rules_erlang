load("//:erlang_app_info.bzl", "ErlangAppInfo")
load("//:util.bzl", "path_join")
load(":util.bzl", "erl_libs_contents")
load(
    ":erlang_installation.bzl",
    "ErlangInstallationInfo",
    "erlang_dirs",
    "maybe_symlink_erlang",
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

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    compile_first = ""
    if ctx.attr.compile_first != None:
        cf_info = ctx.attr.compile_first[DefaultInfo]
        compile_first = cf_info.files_to_run.executable.path
        runfiles = runfiles.merge(cf_info.default_runfiles)

    script = """set -euo pipefail

{maybe_symlink_erlang}

mkdir -p {dest_dir}

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
        maybe_symlink_erlang = maybe_symlink_erlang(ctx),
        erlang_home = erlang_home,
        dest_dir = dest_dir,
        erl_libs_path = erl_libs_path,
        compile_first = compile_first,
        include_args = " ".join(include_args),
        pa_args = " ".join(pa_args),
        out_dir = dest_dir,
        erlc_opts = " ".join(["'{}'".format(opt) for opt in ctx.attr.erlc_opts]),
    )

    inputs = depset(
        direct = ctx.files.hdrs + ctx.files.srcs + ctx.files.beam + erl_libs_files,
        transitive = [runfiles.files],
    )

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

erlang_bytecode = rule(
    implementation = _impl,
    attrs = {
        "erlang_installation": attr.label(
            mandatory = True,
            providers = [ErlangInstallationInfo],
        ),
        "compile_first": attr.label(
            executable = True,
            cfg = "exec",
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