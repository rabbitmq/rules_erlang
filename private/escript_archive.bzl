load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_install_erlang",
)
load(
    "//:erlang_app_info.bzl",
    "ErlangAppInfo",
    "flat_deps",
)
load(
    "//:util.bzl",
    "path_join",
)
load(":transitions.bzl",
"beam_transition")
load(
    ":util.bzl",
    "additional_file_dest_relative_path",
)

DEFAULT_HEADERS = [
    "shebang",
    "comment",
]

def _build_erl_libs(ctx, dir = None):
    deps = flat_deps([ctx.attr.app])

    entries = {}
    for dep in deps:
        lib_info = dep[ErlangAppInfo]
        for src in lib_info.beam:
            if src.is_directory:
                fail("beam directories are not supported with this rule")
            archive_path = path_join(lib_info.app_name, "ebin", src.basename)
            dest = ctx.actions.declare_file(path_join(dir, archive_path))
            ctx.actions.symlink(output = dest, target_file = src)
            entries[archive_path] = dest
        for src in lib_info.priv:
            rp = additional_file_dest_relative_path(dep.label, src)
            archive_path = path_join(lib_info.app_name, rp)
            dest = ctx.actions.declare_file(path_join(dir, archive_path))
            ctx.actions.symlink(output = dest, target_file = src)
            entries[archive_path] = dest
    return entries

def _impl(ctx):
    name = ctx.attr.out if ctx.attr.out != "" else ctx.label.name

    output = ctx.actions.declare_file(name)

    file_entries = [
        "{{filename:basename(\"{p}\"), \"{p}\"}}".format(p = src.path)
        for src in ctx.files.srcs + ctx.files.beam
    ]

    app_entries = []
    app_files = []
    if ctx.attr.app != None:
        entries = _build_erl_libs(ctx, dir = ctx.label.name + "_deps")
        for path, file in entries.items():
            app_entries.append("{{\"{}\", \"{}\"}}".format(path, file.path))
            app_files.append(file)

    entry_list = "[" + ", ".join(app_entries + file_entries) + "]"

    (erlang_home, _, runfiles) = erlang_dirs(ctx)

    script = """set -euo pipefail

{maybe_install_erlang}

"{erlang_home}"/bin/erl \\
    -noshell \\
    -eval 'io:format("Assembling ~s escript...~n", ["{name}"]),
ArchiveEntries = [begin
    {{ok, Bin}} = file:read_file(Path),
    {{Name, Bin}}
end || {{Name, Path}} <- {entry_list}],
ok = escript:create("{output}",
                    [{headers}
                     {{archive, ArchiveEntries, []}}]),
io:format("done.~n", []),
halt().
'
""".format(
        maybe_install_erlang = maybe_install_erlang(ctx),
        erlang_home = erlang_home,
        name = name,
        headers = "".join(["{}, ".format(h) for h in ctx.attr.headers]),
        entry_list = entry_list,
        output = output.path,
    )

    inputs = depset(
        direct = app_files + ctx.files.srcs + ctx.files.beam,
        transitive = [runfiles.files],
    )

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = [output],
        command = script,
    )

    return [
        DefaultInfo(
            executable = output,
            runfiles = runfiles,
        ),
    ]

escript_archive = rule(
    implementation = _impl,
    attrs = {
        "out": attr.string(),
        "headers": attr.string_list(
            default = DEFAULT_HEADERS,
        ),
        "srcs": attr.label_list(allow_files = [".erl"]),
        "beam": attr.label_list(
            allow_files = [".beam"],
            cfg = beam_transition,
        ),
        "app": attr.label(providers = [ErlangAppInfo]),
        # This attribute is required to use starlark transitions. It allows
        # allowlisting usage of this rule. For more information, see
        # https://docs.bazel.build/versions/master/skylark/config.html#user-defined-transitions
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist",
        ),
    },
    toolchains = ["//tools:toolchain_type"],
    executable = True,
)
