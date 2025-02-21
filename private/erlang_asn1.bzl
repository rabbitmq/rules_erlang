load("//:erlang_app_info.bzl", "ErlangAppInfo")
load("//:util.bzl", "path_join")
load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
)
load(":erlang_app_sources.bzl", "ErlangAppSourcesInfo", "ErlangGeneratedCodeInfo")
load(":util.bzl", "erl_libs_contents")

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
    package_dir = path_join(
        ctx.label.workspace_root,
        ctx.label.package,
    )

    include_args = []
    if package_dir != "":
        include_args.extend(["-I", package_dir])
    for dir in unique_dirnames(ctx.files.incs):
        include_args.extend(["-I", dir])

    srcs = ctx.actions.args()
    srcs.add_all(ctx.files.srcs)

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    src_outputs = []
    include_outputs = []
    for src in ctx.files.srcs:
        fname = src.basename.rsplit(".", 1)[0].removesuffix(".set")
        src_outputs.append(ctx.actions.declare_file(path_join("src", fname + ".erl")))
        include_outputs.append(ctx.actions.declare_file(path_join("include", fname + ".hrl")))

    script = """set -euo pipefail

TMP=$(mktemp -d || mktemp -d -t bazel-tmp)
test -d "$TMP"
trap "rm -fr '$TMP'" EXIT

{maybe_install_erlang}

mkdir -p {src_out} {include_out}

ERLC_OPTS="{erlc_opts}"

"{erlang_home}"/bin/erlc \\
    +noobj \\
    {include_args} \\
    -o "$TMP" \\
    $ERLC_OPTS $@

wrap_ext() {{
    IS_EXT=0
    while IFS="" read L ; do
        case "$IS_EXT:$L" in
            "0:-record('EXTERNAL',"*)
                printf "%s\\n" \\
                    "-ifndef(_EXTERNAL_HRL_)." \\
                    "-define(_EXTERNAL_HRL_, true)." \\
                    "$L"
                IS_EXT=1
                ;;
            1:*")."*)
                printf "%s\\n" \\
                    "$L" \\
                    "-endif."
                IS_EXT=0
                ;;
            *)
                printf "%s\\n" "$L"
                ;;
        esac
    done
}}

for F in $@ ; do
    FNAME=$(basename "${{F}}")
    FNAME="${{FNAME%.*}}"
    FNAME="$(basename "${{FNAME}}" .set)"
    mv "$TMP/$FNAME.erl" "{src_out}/$FNAME.erl"
    if [ -f "$TMP/$FNAME.hrl" ] ; then
        wrap_ext < "$TMP/$FNAME.hrl" > "{include_out}/$FNAME.hrl"
    else
        echo '%% dummy' > "{include_out}/$FNAME.hrl"
    fi
done
    """.format(
        maybe_install_erlang = maybe_install_erlang(ctx),
        erlang_home = erlang_home,
        erlc_opts = " ".join(ctx.attr.erlc_opts),
        src_out = src_outputs[0].dirname,
        include_out = include_outputs[0].dirname,
        erl_libs_path = "",
        include_args = " ".join(include_args),
    )

    inputs = depset(
        direct = ctx.files.srcs + ctx.files.incs,
        transitive = [runfiles.files],
    )

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = src_outputs + include_outputs,
        command = script,
        arguments = [srcs],
        mnemonic = "ERLCASN1",
    )

    return [
        ErlangGeneratedCodeInfo(
            srcs = depset(src_outputs),
            includes = depset(include_outputs),
        ),
        DefaultInfo(files = depset(src_outputs + include_outputs)),
    ]

erlang_asn1 = rule(
    implementation = _impl,
    attrs = {
        "srcs": attr.label_list(
            mandatory = True,
            allow_files = [".asn", ".asn1"],
        ),
        "incs": attr.label_list(
            allow_files = [".asn", ".asn1"],
        ),
        "deps": attr.label_list(
            providers = [ErlangAppInfo],
        ),
        "erlc_opts": attr.string_list(),
    },
    toolchains = ["//tools:toolchain_type"],
    provides = [ErlangGeneratedCodeInfo],
)
