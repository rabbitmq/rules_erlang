load(":erlang_home.bzl", "ErlangVersionProvider")

ErlangAppInfo = provider(
    doc = "Compiled Erlang Application",
    fields = {
        "app_name": "Name of the erlang application",
        "erlang_version": "The erlang version used to produce the beam files",
        "include": "Public header files",
        "beam": "Compiled bytecode (.beam) files, or a single ebin directory",
        "priv": "Additional files",
        "license_files": "License files",
        "deps": "Runtime dependencies of the compiled sources",
    },
)

def _contains_by_app_name(dep, deps):
    for d in deps:
        if d[ErlangAppInfo].app_name == dep[ErlangAppInfo].app_name:
            # TODO: fail if name matches but they are not identical
            return True
    return False

def flat_deps(list_of_labels_providing_erlang_lib_info):
    deps = []
    for dep in list_of_labels_providing_erlang_lib_info:
        if not _contains_by_app_name(dep, deps):
            deps.append(dep)
            for t in dep[ErlangAppInfo].deps:
                if not _contains_by_app_name(t, deps):
                    deps.append(t)
    return deps

def _impl(ctx):
    compiled_files = ctx.files.app + ctx.files.beam

    deps = flat_deps(ctx.attr.deps)

    runfiles = ctx.runfiles(compiled_files + ctx.files.priv)
    for dep in ctx.attr.deps:
        runfiles = runfiles.merge(dep[DefaultInfo].default_runfiles)

    return [
        ErlangAppInfo(
            app_name = ctx.attr.app_name,
            erlang_version = ctx.attr._erlang_version[ErlangVersionProvider].version,
            include = ctx.files.hdrs,
            beam = compiled_files,
            priv = ctx.files.priv,
            license_files = ctx.files.license_files,
            deps = deps,
        ),
        DefaultInfo(
            files = depset(compiled_files),
            runfiles = runfiles,
        ),
    ]

erlang_app_info = rule(
    implementation = _impl,
    attrs = {
        "_erlang_version": attr.label(default = ":erlang_version"),
        "app_name": attr.string(mandatory = True),
        "hdrs": attr.label_list(allow_files = [".hrl"]),
        "app": attr.label(allow_files = [".app"]),
        "beam": attr.label_list(allow_files = [".beam", ".appup"]),
        "priv": attr.label_list(allow_files = True),
        "license_files": attr.label_list(allow_files = True),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
    },
)
