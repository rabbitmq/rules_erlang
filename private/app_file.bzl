load("//:erlang_app_info.bzl", "ErlangAppInfo")
load("//:util.bzl", "path_join")
load(
    "//tools:erlang_toolchain.bzl",
    "erlang_dirs",
    "maybe_symlink_erlang",
)

def _module_name(f):
    return "'{}'".format(f.basename.replace(".beam", "", 1))

def _impl(ctx):
    app_file = ctx.actions.declare_file(
        path_join(ctx.attr.dest, "{}.app".format(ctx.attr.app_name)),
    )

    if len(ctx.files.app_src) > 1:
        fail("Multiple .app.src files ({}) are not supported".format(
            ctx.files.app_src,
        ))
    elif len(ctx.files.app_src) == 1:
        src = ctx.files.app_src[0].path
    else:
        src = ""

    app_module = ctx.attr.app_module if ctx.attr.app_module != "" else ctx.attr.app_name + "_app"
    if len([m for m in ctx.files.modules if m.basename == app_module + ".beam"]) != 1:
        app_module = ""

    modules = "[" + ",".join([_module_name(m) for m in ctx.files.modules]) + "]"

    if len(ctx.attr.app_registered) > 0:
        registered_list = "[" + ",".join([ctx.attr.app_name + "_sup"] + ctx.attr.app_registered) + "]"
    else:
        registered_list = ""

    applications = ["kernel", "stdlib"] + ctx.attr.extra_apps
    for dep in ctx.attr.deps:
        applications.append(dep[ErlangAppInfo].app_name)
    applications_list = "[" + ",".join(applications) + "]"

    stamp = ctx.attr.stamp == 1 or (ctx.attr.stamp == -1 and
                                    ctx.attr.private_stamp_detect)

    (erlang_home, erlang_release_dir, runfiles) = erlang_dirs(ctx)

    app_file_tool_path = ctx.attr.app_file_tool[DefaultInfo].files_to_run.executable.path

    script = """set -euo pipefail

{maybe_symlink_erlang}

if [ -n "{src}" ]; then
    cp {src} {out}
else
    echo "{{application,'{name}',[{{registered, ['{name}_sup']}},{{env, []}}]}}." > {out}
fi

if [ -n '{description}' ]; then
    cat << 'EOF' | "{erlang_home}"/bin/escript {app_file_tool} description {out} > {out}.tmp && mv {out}.tmp {out}
"{description}".
EOF
fi

# set the version env var from the stable-status.txt
# if stamping...
if [ -n "{stamp_version_key}" ]; then
    if [ -f "{info_file}" ]; then
        while IFS=' ' read -r key val ; do
            if [ "$key" == "STABLE_{stamp_version_key}" ]; then
                VSN=$val
            fi
        done < {info_file}
    else
        echo "Stamping requested but {info_file} is not available"
        exit 1
    fi
else
    VSN={version}
fi
if [ -n "$VSN" ]; then
    echo "\\"$VSN\\"." | \\
        "{erlang_home}"/bin/escript {app_file_tool} \\
        vsn \\
        {out} > {out}.tmp && mv {out}.tmp {out}
fi

cat << 'EOF' | "{erlang_home}"/bin/escript {app_file_tool} modules {out} > {out}.tmp && mv {out}.tmp {out}
{modules}.
EOF

if [ -n '{registered}' ]; then
    cat << 'EOF' | "{erlang_home}"/bin/escript {app_file_tool} registered {out} > {out}.tmp && mv {out}.tmp {out}
{registered}.
EOF
fi

if [ -n '{applications}' ]; then
    cat << 'EOF' | "{erlang_home}"/bin/escript {app_file_tool} applications {out} > {out}.tmp && mv {out}.tmp {out}
{applications}.
EOF
fi

if [ -n '{app_module}' ]; then
    cat << 'EOF' | "{erlang_home}"/bin/escript {app_file_tool} mod {out} > {out}.tmp && mv {out}.tmp {out}
{{{app_module}, []}}.
EOF
fi

if [ -n '{env}' ]; then
    cat << 'EOF' | "{erlang_home}"/bin/escript {app_file_tool} env {out} > {out}.tmp && mv {out}.tmp {out}
{env}.
EOF
fi

if [ -n '{extra_keys}' ]; then
    cat << 'EOF' | "{erlang_home}"/bin/escript {app_file_tool} {out} > {out}.tmp && mv {out}.tmp {out}
[
{extra_keys}
].
EOF
fi
""".format(
        maybe_symlink_erlang = maybe_symlink_erlang(ctx),
        erlang_home = erlang_home,
        app_file_tool = app_file_tool_path,
        info_file = ctx.info_file.path,
        name = ctx.attr.app_name,
        description = ctx.attr.app_description,
        stamp_version_key = ctx.attr.stamp_version_key if stamp else "",
        version = ctx.attr.app_version,
        modules = modules,
        registered = registered_list,
        applications = applications_list,
        app_module = app_module,
        env = ctx.attr.app_env,
        extra_keys = ctx.attr.app_extra_keys,
        src = src,
        out = app_file.path,
    )

    runfiles = runfiles.merge(
        ctx.attr.app_file_tool[DefaultInfo].default_runfiles,
    )

    inputs = depset(
        direct = ctx.files.app_src + ([ctx.info_file] if stamp else []),
        transitive = [runfiles.files],
    )

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = [app_file],
        command = script,
        mnemonic = "APP",
    )

    return [
        DefaultInfo(files = depset([app_file])),
    ]

app_file = rule(
    implementation = _impl,
    attrs = {
        "app_file_tool": attr.label(
            mandatory = True,
            executable = True,
            cfg = "exec",
        ),
        "app_name": attr.string(mandatory = True),
        "app_version": attr.string(),
        "app_description": attr.string(),
        "app_module": attr.string(),
        "app_registered": attr.string_list(),
        "app_env": attr.string(),
        "app_extra_keys": attr.string(),
        "extra_apps": attr.string_list(),
        "app_src": attr.label_list(allow_files = [".app.src"]),
        "modules": attr.label_list(allow_files = [".beam"]),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
        "dest": attr.string(
            default = "ebin",
        ),
        "stamp": attr.int(default = -1),
        "stamp_version_key": attr.string(
            default = "GIT_COMMIT",
            doc = """This will inject the value with this key from
stable-status.txt as the app version if the build is stamped""",
        ),
        # Is --stamp set on the command line?
        # TODO(https://github.com/bazelbuild/rules_pkg/issues/340): Remove this.
        "private_stamp_detect": attr.bool(default = False),
    },
    toolchains = ["//tools:toolchain_type"],
)
