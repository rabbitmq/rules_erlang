load(
    "//private:erlang_installation.bzl",
    "ErlangInstallationInfo",
    "erlang_dirs",
    "maybe_symlink_erlang",
)

def _impl(ctx):
    out = ctx.actions.declare_file(ctx.attr.out if ctx.attr.out != "" else ctx.label.name)

    source_entries = [
        "{{source,\"{}\"}}".format(f.path)
        for f in ctx.files.srcs
    ]

    beam_entries = [
        "{{beam,\"{}\"}}".format(f.path)
        for f in ctx.files.beam
    ]

    entries = ",".join(source_entries + beam_entries)

    args = ctx.actions.args()
    args.add("""io:format("Assembiling {out} escript...~n", []),
ok = escript:create("{out}",
                    [shebang, comment,
                    {entries}]),
io:format("done.~n", []),
halt().
""".format(
        out = out.path,
        source_entries = source_entries,
        entries = entries,
    ))

    (erlang_home, erlang_release_dir, runfiles) = erlang_dirs(ctx)

    inputs = depset(
        direct = ctx.files.srcs + ctx.files.beam,
        transitive = [runfiles.files],
    )

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = [out],
        command = """set -euo pipefail

{maybe_symlink_erlang}

"{erlang_home}"/bin/erl -noshell -eval "$@"
""".format(
            maybe_symlink_erlang = maybe_symlink_erlang(ctx),
            erlang_home = erlang_home,
        ),
        arguments = [args],
    )

    return [
        DefaultInfo(
            executable = out,
        ),
    ]

escript_flat = rule(
    implementation = _impl,
    attrs = {
        "erlang_installation": attr.label(
            mandatory = True,
            providers = [ErlangInstallationInfo],
        ),
        "srcs": attr.label_list(
            allow_files = [".erl"],
        ),
        "beam": attr.label_list(
            allow_files = [".beam"],
        ),
        "out": attr.string(),
    },
)
