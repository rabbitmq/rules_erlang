load("//:erlang_app_info.bzl", "ErlangAppInfo")
load("//:util.bzl", "path_join")

def _module_name(f):
    return "'{}'".format(f.basename.replace(".beam", "", 1))

def _impl(ctx):
    app_file = ctx.actions.declare_file(
        path_join("ebin", "{}.app".format(ctx.attr.app_name)),
    )

    if len(ctx.files.app_src) > 1:
        fail("Multiple .app.src files ({}) are not supported".format(ctx.files.app_src))

    modules_list = "[" + ",".join([_module_name(m) for m in ctx.files.modules]) + "]"

    if len(ctx.files.app_src) == 1:
        modules_term = "{modules," + modules_list + "}"

        # TODO: handle the data structure manipulation with erlang itself
        ctx.actions.expand_template(
            template = ctx.files.app_src[0],
            output = app_file,
            substitutions = {
                "{modules,[]}": modules_term,
                "{modules, []}": modules_term,
            },
        )
    else:
        if ctx.attr.app_version == "":
            fail("app_version must be set when app_src is empty")

        app_module = ctx.attr.app_module if ctx.attr.app_module != "" else ctx.attr.app_name + "_app"

        if len([m for m in ctx.files.modules if m.basename == app_module + ".beam"]) == 1:
            template = ctx.file._app_with_mod_file_template
        else:
            template = ctx.file._app_file_template

        project_description = ctx.attr.app_description if ctx.attr.app_description != "" else ctx.attr.app_name

        registered_list = "[" + ",".join([ctx.attr.app_name + "_sup"] + ctx.attr.app_registered) + "]"

        applications = ["kernel", "stdlib"] + ctx.attr.extra_apps
        for dep in ctx.attr.deps:
            applications.append(dep[ErlangAppInfo].app_name)
        applications_list = "[" + ",".join(applications) + "]"

        app_extra_keys_terms = ""
        if len(ctx.attr.app_extra_keys) > 0:
            app_extra_keys_terms = ",\n" + ",\n".join(["\t" + t for t in ctx.attr.app_extra_keys])

        ctx.actions.expand_template(
            template = template,
            output = app_file,
            substitutions = {
                "$(PROJECT)": ctx.attr.app_name,
                "$(PROJECT_DESCRIPTION)": project_description,
                "$(PROJECT_VERSION)": ctx.attr.app_version,
                "$(PROJECT_ID_TERM)": "",
                "$(MODULES_LIST)": modules_list,
                "$(REGISTERED_LIST)": registered_list,
                "$(APPLICATIONS_LIST)": applications_list,
                "$(PROJECT_MOD)": app_module,
                "$(PROJECT_ENV)": ctx.attr.app_env,
                "$(PROJECT_APP_EXTRA_KEYS)": app_extra_keys_terms,
            },
        )

    return [
        DefaultInfo(files = depset([app_file])),
    ]

app_file_private = rule(
    implementation = _impl,
    attrs = {
        "_erlang_home": attr.label(default = Label("//:erlang_home")),
        "_erlang_version": attr.label(default = Label("//:erlang_version")),
        "_app_file_template": attr.label(
            default = Label("//private:app_file.template"),
            allow_single_file = True,
        ),
        "_app_with_mod_file_template": attr.label(
            default = Label("//private:app_with_mod_file.template"),
            allow_single_file = True,
        ),
        "app_name": attr.string(mandatory = True),
        "app_version": attr.string(),
        "app_description": attr.string(),
        "app_module": attr.string(),
        "app_registered": attr.string_list(),
        "app_env": attr.string(default = "[]"),
        "app_extra_keys": attr.string_list(),
        "extra_apps": attr.string_list(),
        "app_src": attr.label_list(allow_files = [".app.src"]),
        "modules": attr.label_list(allow_files = [".beam"]),
        "deps": attr.label_list(providers = [ErlangAppInfo]),
    },
)
