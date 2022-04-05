load("//:erlang_home.bzl", "ErlangHomeProvider", "ErlangVersionProvider")
load("//:erlang_app_info.bzl", "ErlangAppInfo", "flat_deps")
load(
    "//:util.bzl",
    "BEGINS_WITH_FUN",
    "QUERY_ERL_VERSION",
    "path_join",
)
load(":util.bzl", "additional_file_dest_relative_path")

DEFAULT_HEADERS = [
    "shebang",
    "comment",
]

def _build_erl_libs(ctx, dir = None):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    deps = flat_deps([ctx.attr.app])

    entries = {}
    for dep in deps:
        lib_info = dep[ErlangAppInfo]
        if lib_info.erlang_version != erlang_version:
            fail("Mismatched erlang versions", erlang_version, lib_info.erlang_version)
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

    script = """set -euo pipefail

{begins_with_fun}
V=$("{erlang_home}"/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

"{erlang_home}"/bin/erl \\
    -noshell \\
    -eval 'io:format("Assembling {name} escript...~n", []),
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
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
        erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
        name = name,
        headers = "".join(["{}, ".format(h) for h in ctx.attr.headers]),
        entry_list = entry_list,
        output = output.path,
    )

    inputs = app_files + ctx.files.srcs + ctx.files.beam

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = [output],
        command = script,
    )

    return [
        DefaultInfo(executable = output),
    ]

escript_private = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
        "out": attr.string(),
        "headers": attr.string_list(
            default = DEFAULT_HEADERS,
        ),
        "srcs": attr.label_list(allow_files = [".erl"]),
        "beam": attr.label_list(allow_files = [".beam"]),
        "app": attr.label(providers = [ErlangAppInfo]),
    },
    executable = True,
)
