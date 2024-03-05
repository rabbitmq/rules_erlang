def _impl(repository_ctx):
    build_file_content = """\
load("@rules_erlang//gmake:gmake_toolchain.bzl", "gmake_toolchain")

"""

    for name, path in repository_ctx.attr.gmakes.items():
        build_file_content += """\
gmake_toolchain(
    name = "{name}",
    gmake_path = "{path}",
)

toolchain(
    name = "{name}_toolchain",
    toolchain = ":{name}",
    toolchain_type = "@rules_erlang//gmake:toolchain_type",
    visibility = ["//visibility:public"],
)

""".format(
            name = name,
            path = path,
        )

    repository_ctx.file(
        "BUILD.bazel",
        build_file_content,
    )

gmake_config = repository_rule(
    implementation = _impl,
    attrs = {
        "gmakes": attr.string_dict(),
    },
    # environ = [
    #     "MAKE",
    # ],
    # local = True,
)
