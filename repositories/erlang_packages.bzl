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

    repository_ctx.template(
        "BUILD.bazel",
        repository_ctx.attr._template,
        {
            "%{APPS}": _to_string_list(apps_srcs),
            "%{TEST_APPS}": _to_string_list(test_apps_srcs),
        },
        False,
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
