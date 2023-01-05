load(
    "//:erlang_app_info.bzl",
    "ErlangAppInfo",
    "flat_deps",
)
load(
    "//:util.bzl",
    "path_join",
)

_DEFAULT_ERL_LIBS_DIR = "deps"

def additional_file_dest_relative_path(dep_label, f):
    if dep_label.workspace_root != "":
        workspace_root = dep_label.workspace_root.replace("external/", "../")
        rel_base = path_join(workspace_root, dep_label.package)
    else:
        rel_base = dep_label.package
    if rel_base != "":
        return f.short_path.replace(rel_base + "/", "")
    else:
        return f.short_path

def erl_libs_contents2(ctx, target_info = None, deps = [], headers = False, dir = _DEFAULT_ERL_LIBS_DIR):
    erl_libs_files = []
    if headers and target_info != None:
        dep_path = path_join(dir, target_info.app_name)
        for hdr in target_info.include:
            dest = ctx.actions.declare_file(path_join(dep_path, hdr.path))
            ctx.actions.symlink(output = dest, target_file = hdr)
            erl_libs_files.append(dest)
    for dep in deps:
        lib_info = dep[ErlangAppInfo]
        dep_path = path_join(dir, lib_info.app_name)
        if headers:
            for hdr in lib_info.include:
                rp = additional_file_dest_relative_path(dep.label, hdr)
                dest = ctx.actions.declare_file(path_join(dep_path, rp))
                ctx.actions.symlink(output = dest, target_file = hdr)
                erl_libs_files.append(dest)
        for src in lib_info.beam:
            if src.is_directory:
                if len(lib_info.beam) != 1:
                    fail("ErlangAppInfo.beam must be a collection of files, or a single ebin dir")
                dest = ctx.actions.declare_directory(path_join(dep_path, "ebin"))
                ctx.actions.run_shell(
                    inputs = [src],
                    outputs = [dest],
                    command = "cp -R \"{}\"/* \"{}\"".format(src.path, dest.path),
                )
            else:
                dest = ctx.actions.declare_file(path_join(dep_path, "ebin", src.basename))
                ctx.actions.symlink(output = dest, target_file = src)
            erl_libs_files.append(dest)
        for src in lib_info.priv:
            rp = additional_file_dest_relative_path(dep.label, src)
            dest = ctx.actions.declare_file(path_join(dep_path, rp))
            ctx.actions.symlink(output = dest, target_file = src)
            erl_libs_files.append(dest)
    return erl_libs_files

def erl_libs_contents(ctx, transitive = True, **kwargs):
    if transitive:
        deps = flat_deps(ctx.attr.deps)
    else:
        deps = ctx.attr.deps

    return erl_libs_contents2(
        ctx,
        deps = deps,
        **kwargs
    )

def to_erlang_string_list(strings):
    return "[" + ",".join(["\"{}\"".format(s) for s in strings]) + "]"

def to_erlang_atom_list(strings):
    return "[" + ",".join(strings) + "]"
