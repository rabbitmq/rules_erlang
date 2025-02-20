def _to_string_list(strings):
    if len(strings) == 0:
        return "[]"
    return "[\n%s\n    ]" % "\n".join(['        "%s",' % s for s in strings])

def _impl(repository_ctx):
    apps_srcs = [
        "@{}//:srcs".format(app)
        for app in repository_ctx.attr.apps
    ]
    test_apps_srcs = [
        "@{}//:srcs".format(app)
        for app in repository_ctx.attr.test_apps
    ]

    BUILD_FILE_CONTENT = """\
load("@rules_erlang//:compile_many.bzl", "compile_many")

package(
    default_visibility = ["//visibility:public"],
)

compile_many(
    name = "deps",
    apps = {apps},
)

compile_many(
    name = "test_deps",
    apps = {test_apps},
    testonly = True,
)
""".format(
        apps = _to_string_list(apps_srcs),
        test_apps = _to_string_list(test_apps_srcs),
    )

    repository_ctx.file(
        "BUILD.bazel",
        content = BUILD_FILE_CONTENT,
    )

    for app, deps in repository_ctx.attr.apps.items():
        if app in repository_ctx.attr.apps_with_metadata:
            BUILD_FILE_CONTENT = APP_BUILD_FILE_METADATA_TEMPLATE.format(
                app_name = app,
                erl_libs = "//:deps",
                testonly = False,
                deps = _to_string_list(deps),
            )
        else:
            dep_labels = _to_string_list(["//{}".format(dep) for dep in deps])
            BUILD_FILE_CONTENT = APP_BUILD_FILE_TEMPLATE.format(
                app_name = app,
                erl_libs = "//:deps",
                testonly = False,
                deps = dep_labels,
            )

        repository_ctx.file(
            "{}/BUILD.bazel".format(app),
            content = BUILD_FILE_CONTENT,
        )

    for test_app, deps in repository_ctx.attr.test_apps.items():
        if test_app in repository_ctx.attr.apps_with_metadata:
            BUILD_FILE_CONTENT = APP_BUILD_FILE_METADATA_TEMPLATE.format(
                app_name = test_app,
                erl_libs = "//:test_deps",
                testonly = True,
                deps = _to_string_list(deps),
            )
        else:
            dep_labels = _to_string_list(["//{}".format(dep) for dep in deps])
            BUILD_FILE_CONTENT = APP_BUILD_FILE_TEMPLATE.format(
                app_name = test_app,
                erl_libs = "//:test_deps",
                testonly = True,
                deps = dep_labels,
            )

        repository_ctx.file(
            "{}/BUILD.bazel".format(test_app),
            content = BUILD_FILE_CONTENT,
        )

erlang_packages = repository_rule(
    implementation = _impl,
    attrs = {
        "apps": attr.string_list_dict(),
        "test_apps": attr.string_list_dict(),
        "apps_with_metadata": attr.string_list(),
    },
)

APP_BUILD_FILE_TEMPLATE = """\
load("@rules_erlang//:extract_app.bzl", "extract_app")

extract_app(
    name = "{app_name}",
    app_name = "{app_name}",
    erl_libs = "{erl_libs}",
    copy_headers = True,
    testonly = {testonly},
    verify = False,
    deps = {deps},
    visibility = ["//visibility:public"],
)
"""

APP_BUILD_FILE_METADATA_TEMPLATE = """\
load("@rules_erlang//:extract_app.bzl", "extract_app")
load("@rules_erlang//repositories:hex_metadata_parser.bzl", "metadata_to_deps")
load("@{app_name}//:metadata.bzl", "METADATA")

extract_app(
    name = "{app_name}",
    app_name = "{app_name}",
    erl_libs = "{erl_libs}",
    copy_headers = True,
    testonly = {testonly},
    verify = False,
    deps = metadata_to_deps(METADATA, {deps}),
    visibility = ["//visibility:public"],
)
"""
