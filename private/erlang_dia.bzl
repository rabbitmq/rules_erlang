load("//:erlang_app_info.bzl", "ErlangAppInfo")
load("//:util.bzl", "path_join")
load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
)
load(":compile_many.bzl", "CompileManyInfo")
load(":erlang_app_sources.bzl", "ErlangGeneratedCodeInfo")
load(":util.bzl", "erl_libs_contents")

def unique_dirnames(files):
    dirs = []
    for f in files:
        dirname = f.path if f.is_directory else f.dirname
        if dirname not in dirs:
            dirs.append(dirname)
    return dirs

def _impl(ctx):
    package_dir = path_join(
        ctx.label.workspace_root,
        ctx.label.package,
    )

    beams = []
    for lib in ctx.attr.erl_libs:
        for app in lib[CompileManyInfo].apps.values():
            beams.extend([o for o in app.outs if o.path.endswith(".beam")])
    include_dirs = unique_dirnames(beams)

    srcs = ctx.actions.args()
    srcs.add_all(ctx.files.srcs)

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    src_outputs = []
    include_outputs = []
    for src in ctx.files.srcs:
        fname = src.basename.removesuffix(".dia")
        src_outputs.append(ctx.actions.declare_file(path_join("src", fname + ".erl")))
        include_outputs.append(ctx.actions.declare_file(path_join("include", fname + ".hrl")))

    script = """#!/usr/bin/env bash

set -euo pipefail

TMP=$(mktemp -d || mktemp -d -t bazel-tmp)
test -d "$TMP"
trap "rm -fr '$TMP'" EXIT

{maybe_install_erlang}

mkdir -p {src_out} {include_out}

SRC_DICTS="$@"
INCLUDE_OPTS='{include_opts}'
CODEC_OPTS='{codec_opts}'

SRC_ERLLIST=$(for D in $SRC_DICTS; do echo "\\\"$D\\\","; done)
SRC_ERLLIST=${{SRC_ERLLIST%,}}

DIA_OPTS="[$CODEC_OPTS]++[$INCLUDE_OPTS]++[{{outdir,\\"$TMP\\"}}]"

"{erlang_home}"/bin/erl \\
    -noshell \\
    -eval "[ok = diameter_make:codec(Src, $DIA_OPTS) || Src <- [$SRC_ERLLIST]]" \\
    -s erlang halt

for SRC_DICT in $SRC_DICTS;
do
    FNAME="$(basename "${{SRC_DICT}}" .dia)"
    mv "$TMP/$FNAME.erl" {src_out}
    mv "$TMP/$FNAME.hrl" {include_out}
done
    """.format(
        maybe_install_erlang = maybe_install_erlang(ctx),
        erlang_home = erlang_home,
        src_out = src_outputs[0].dirname,
        include_out = include_outputs[0].dirname,
        erl_libs_path = "",
        codec_opts = ",".join(ctx.attr.codec_opts),
        include_opts = ",".join(["{include,\"%s\"}" % d for d in include_dirs]),
    )

    inputs = depset(
        direct = ctx.files.srcs + beams,
        transitive = [runfiles.files],
    )

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = src_outputs + include_outputs,
        command = script,
        arguments = [srcs],
        mnemonic = "ERLDIA",
    )

    return [
        ErlangGeneratedCodeInfo(
            srcs = depset(src_outputs),
            includes = depset(include_outputs),
        ),
        DefaultInfo(files = depset(src_outputs + include_outputs)),
    ]

erlang_dia = rule(
    implementation = _impl,
    attrs = {
        "srcs": attr.label_list(
            mandatory = True,
            doc = "The dia files to compile",
            allow_files = [".dia"],
        ),
        "erl_libs": attr.label_list(
            doc = "Compiled apps with beam files for the inherited dictionaries",
            providers = [CompileManyInfo],
        ),
        "deps": attr.label_list(
            providers = [ErlangAppInfo],
        ),
        "codec_opts": attr.string_list(
            doc = "Transformation options for diameter_make:codec (only use 'name', 'prefix' or 'inherits' otions)",
        ),
    },
    toolchains = ["//tools:toolchain_type"],
    provides = [ErlangGeneratedCodeInfo],
)
