load(
    ":erlang_home.bzl",
    "ErlangHomeProvider",
    "ErlangVersionProvider",
)
load(
    ":bazel_erlang_lib.bzl",
    "BEGINS_WITH_FUN",
    "ErlangLibInfo",
    "QUERY_ERL_VERSION",
    "path_join",
)
load(":ct.bzl", "code_paths")

def _to_erlang_string_list(strings):
    return "[" + ",".join(["\"{}\"".format(s) for s in strings]) + "]"

def _to_erlang_atom_list(strings):
    return "[" + ",".join(strings) + "]"

def _impl(ctx):
    erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version

    lib_info = ctx.attr.target[ErlangLibInfo]

    if lib_info.erlang_version != erlang_version:
        fail("Erlang version mismatch ({} != {})".format(
            lib_info.erlang_version,
            ctx.attr._erlang_version,
        ))

    extra_paths = []
    dirs = [path_join(ctx.attr.target.label.package, "ebin")]
    for dep in lib_info.deps + ctx.attr.additional_libs:
        if dep.label.workspace_root != "":
            extra_paths.extend(code_paths(dep))
        else:
            dirs.append(path_join(dep.label.package, "ebin"))

    xref_config = "[{xref, ["
    xref_config = xref_config + "{config, #{"
    xref_config = xref_config + "extra_paths => " + _to_erlang_string_list(extra_paths)
    xref_config = xref_config + ", "
    xref_config = xref_config + "dirs => " + _to_erlang_string_list(dirs)
    xref_config = xref_config + "}}"
    xref_config = xref_config + ", "
    xref_config = xref_config + "{checks, " + _to_erlang_atom_list(ctx.attr.checks) + "}"
    xref_config = xref_config + "]}]."
    xref_config = xref_config + "\n"

    config_file = ctx.actions.declare_file("xref.config")
    ctx.actions.write(
        output = config_file,
        content = xref_config,
    )

    xrefr_path = path_join(
        ctx.attr._xrefr.label.workspace_root,
        ctx.file._xrefr.short_path,
    )

    script = """set -euxo pipefail

export HOME=${{TEST_TMPDIR}}

{begins_with_fun}
V=$({erlang_home}/bin/{query_erlang_version})
if ! beginswith "{erlang_version}" "$V"; then
    echo "Erlang version mismatch (Expected {erlang_version}, found $V)"
    exit 1
fi

{erlang_home}/bin/erl \\
    -eval '{{ok, [C]}} = file:consult("{config_path}"), io:format("~p~n", [C]), halt().' \\
    -noshell

$TEST_SRCDIR/$TEST_WORKSPACE/{xrefr} --config {config_path}
""".format(
        begins_with_fun = BEGINS_WITH_FUN,
        query_erlang_version = QUERY_ERL_VERSION,
        package = ctx.label.package,
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
        erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
        xrefr = xrefr_path,
        config_path = config_file.short_path,
    )

    ctx.actions.write(
        output = ctx.outputs.executable,
        content = script,
    )

    runfiles = ctx.runfiles([ctx.file._xrefr, config_file])
    runfiles = runfiles.merge(ctx.attr.target[DefaultInfo].default_runfiles)
    for dep in ctx.attr.additional_libs:
        runfiles = runfiles.merge(dep[DefaultInfo].default_runfiles)
    return [DefaultInfo(runfiles = runfiles)]

xref_test = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(
            default = Label("//:erlang_home"),
        ),
        "_erlang_version": attr.label(
            default = Label("//:erlang_version"),
        ),
        "_xrefr": attr.label(
            default = Label("@xrefr//file"),
            executable = True,
            cfg = "target",
            allow_single_file = True,
        ),
        "target": attr.label(
            providers = [ErlangLibInfo],
            mandatory = True,
        ),
        "checks": attr.string_list(
            default = [
                "undefined_function_calls",
                "locals_not_used",
                "deprecated_function_calls",
            ],
        ),
        "additional_libs": attr.label_list(
            providers = [ErlangLibInfo],
        ),
    },
    test = True,
)

def xref(**kwargs):
    xref_test(
        name = "xref",
        target = ":bazel_erlang_lib",
        **kwargs
    )
