load("//:erlang_home.bzl", "ErlangHomeProvider")
load("//:erlang_app_info.bzl", "ErlangAppInfo")
load("//:util.bzl", "path_join")

APP_FILE_TOOL_SPECIAL_MERGE_KEY = "app_file_tool_special_merge_key"

def _module_name(f):
    return "'{}'".format(f.basename.replace(".beam", "", 1))

def _impl(ctx):
    app_file = ctx.actions.declare_file(
        path_join("ebin", "{}.app".format(ctx.attr.app_name)),
    )

    if len(ctx.files.app_src) > 1:
        fail("Multiple .app.src files ({}) are not supported".format(
            ctx.files.app_src,
        ))
    elif len(ctx.files.app_src) == 1:
        src = ctx.files.app_src[0].path
    else:
        src = ""

    modules = [_module_name(m) for m in ctx.files.modules]

    applications = ["kernel", "stdlib"] + ctx.attr.extra_apps
    for dep in ctx.attr.deps:
        applications.append(dep[ErlangAppInfo].app_name)

    overrides = {
        "modules": modules,
        "applications": applications,
    }

    if ctx.attr.app_version != "":
        overrides["vsn"] = ctx.attr.app_version

    app_module = ctx.attr.app_module if ctx.attr.app_module != "" else ctx.attr.app_name + "_app"
    if len([m for m in ctx.files.modules if m.basename == app_module + ".beam"]) == 1:
        overrides["mod"] = "{" + app_module + ",[]}"

    if len(ctx.attr.app_registered) > 0:
        overrides["registered"] = [ctx.attr.app_name + "_sup"] + ctx.attr.app_registered

    if ctx.attr.app_env != "":
        overrides["env"] = ctx.attr.app_env

    if ctx.attr.app_extra_keys != "":
        overrides[APP_FILE_TOOL_SPECIAL_MERGE_KEY] = ctx.attr.app_extra_keys

    stamp = ctx.attr.stamp == 1 or (ctx.attr.stamp == -1 and
                                    ctx.attr.private_stamp_detect)

    script = """set -euo pipefail

if [ -n "{src}" ]; then
    cp {src} {out}
else
    echo "{{application,'{name}',[{{registered, ['{name}_sup']}},{{env, []}}]}}." > {out}
fi

cat << 'EOF' | "{erlang_home}"/bin/escript {app_file_tool} {out} > {out}.tmp && mv {out}.tmp {out}
{json}
EOF

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
    VSN=""
fi
if [ -n "$VSN" ]; then
    cat << EOF | "{erlang_home}"/bin/escript {app_file_tool} {out} > {out}.tmp && mv {out}.tmp {out}
{{"vsn": "$VSN"}}
EOF
fi
""".format(
        erlang_home = ctx.attr._erlang_home[ErlangHomeProvider].path,
        app_file_tool = ctx.file._app_file_tool_escript.path,
        info_file = ctx.info_file.path,
        name = ctx.attr.app_name,
        stamp_version_key = ctx.attr.stamp_version_key if stamp else "",
        version = ctx.attr.app_version,
        json = json.encode(overrides),
        src = src,
        out = app_file.path,
    )

    inputs = [ctx.file._app_file_tool_escript] + ctx.files.app_src
    if stamp:
        inputs.append(ctx.info_file)

    ctx.actions.run_shell(
        inputs = inputs,
        outputs = [app_file],
        command = script,
        mnemonic = "APP",
    )

    return [
        DefaultInfo(files = depset([app_file])),
    ]

app_file_private = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
        "_app_file_tool_escript": attr.label(
            default = Label("//app_file_tool:escript"),
            allow_single_file = True,
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
)
