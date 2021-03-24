load(":hex_archive.bzl", "hex_archive")

def hex_pm_bazel_erlang_lib(name = None, version = None, erlc_opts = [], first_srcs = [], deps = [], **kwargs):
    hex_archive(
        name = name,
        version = version,
        build_file_content = _BUILD_FILE_TEMPLATE.format(
            app_name = name,
            version = version,
            erlc_opts = erlc_opts,
            first_srcs = first_srcs,
            deps = deps,
        ),
        **kwargs
    )

_BUILD_FILE_TEMPLATE = """
load("@bazel-erlang//:bazel_erlang_lib.bzl", "erlang_lib")

erlang_lib(
    app_name = "{app_name}",
    app_version = "{version}",
    erlc_opts = {erlc_opts},
    first_srcs = {first_srcs},
    deps = {deps},
)
"""
