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

    for app in repository_ctx.attr.apps:
        BUILD_FILE_CONTENT += """\
load("@rules_erlang//:extract_app.bzl", "extract_app")

extract_app(
    name = "{app_name}",
    app_name = "{app_name}",
    erl_libs = ":deps",
    include_headers = True,
    verify = False,
)

""".format(app_name = app)
        repository_ctx.file(
            "{}/BUILD.bazel".format(app),
            content = BUILD_FILE_CONTENT,
        )

    for test_app in repository_ctx.attr.test_apps:
        BUILD_FILE_CONTENT += """\
load("@rules_erlang//:extract_app.bzl", "extract_app")

extract_app(
    name = "{app_name}",
    app_name = "{app_name}",
    erl_libs = ":test_deps",
    include_headers = True,
    testonly = True,
    verify = False,
)

""".format(app_name = test_app)
        repository_ctx.file(
            "{}/BUILD.bazel".format(test_app),
            content = BUILD_FILE_CONTENT,
        )

erlang_packages = repository_rule(
    implementation = _impl,
    attrs = {
        "apps": attr.string_list(),
        "test_apps": attr.string_list(),
        "_template": attr.label(
            default = Label(":BUILD.erlang_packages.tpl"),
        ),
    },
)
