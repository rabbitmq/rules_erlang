def _impl(repository_ctx):
    build_file_content = """\
package(
    default_visibility = ["//visibility:public"],
)

constraint_setting(
    name = "erlang_major_version",
    default_constraint_value = ":erlang_external",
)

constraint_value(
    name = "erlang_external",
    constraint_setting = ":erlang_major_version",
)

platform(
    name = "erlang_any_external",
    constraint_values = [
        ":erlang_external",
    ],
)

"""

    erl_path = repository_ctx.which("erl")
    erl_path_value = '"%s"' % erl_path if erl_path != None else "None"

    repository_ctx.template(
        "external/BUILD.bazel",
        Label("//repositories:BUILD_external.tpl"),
        {
            "%{ERL_PATH_VALUE}": erl_path_value,
            "%{RULES_ERLANG_WORKSPACE}": repository_ctx.attr.rules_erlang_workspace,
        },
        False,
    )

    toolchains = []
    toolchains.append(
        "@{}//external:toolchain".format(
            repository_ctx.name,
        ),
    )

    for name in repository_ctx.attr.versions.keys():
        version = repository_ctx.attr.versions.get(name)
        (major, _, _) = version.partition(".")

        repository_ctx.template(
            "%s/BUILD.bazel" % name,
            Label("//repositories:BUILD_internal.tpl"),
            {
                "%{ERLANG_VERSION}": version,
                "%{URL}": repository_ctx.attr.urls.get(name),
                "%{STRIP_PREFIX}": repository_ctx.attr.strip_prefixs.get(name, ""),
                "%{SHA_256}": repository_ctx.attr.sha256s.get(name, ""),
                "%{ERLANG_MAJOR}": major,
                "%{RULES_ERLANG_WORKSPACE}": repository_ctx.attr.rules_erlang_workspace,
            },
            False,
        )

        toolchains.append(
            "@{}//{}:toolchain".format(
                repository_ctx.name,
                name,
            ),
        )

        if repository_ctx.attr.parent_platform != None:
            parent_string = '"%s"' % repository_ctx.attr.parent_platform
        else:
            parent_string = ""

        build_file_content += """\
constraint_value(
    name = "erlang_{major}",
    constraint_setting = ":erlang_major_version",
)

platform(
    name = "erlang_{major}_internal",
    constraint_values = [
        ":erlang_{major}",
    ],
    parents = [{parent_string}],
)

# platform with no constraints, used to represent
# the ability of erlang bytecode to be run on any
# host with erlang available
platform(
    name = "beam",
    constraint_values = [],
)

""".format(
            major = major,
            parent_string = parent_string,
        )

    repository_ctx.file(
        "BUILD.bazel",
        build_file_content,
        False,
    )

    repository_ctx.template(
        "defaults.bzl",
        Label("//repositories:defaults.bzl.tpl"),
        {
            "%{TOOLCHAINS}": "\n".join([
                '        "%s",' % t
                for t in toolchains
            ]),
        },
        False,
    )

erlang_config = repository_rule(
    implementation = _impl,
    attrs = {
        "rules_erlang_workspace": attr.string(),
        "versions": attr.string_dict(),
        "urls": attr.string_dict(),
        "strip_prefixs": attr.string_dict(),
        "sha256s": attr.string_dict(),
        "parent_platform": attr.label(),
    },
)
