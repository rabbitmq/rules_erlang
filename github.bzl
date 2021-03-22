load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def github_bazel_erlang_lib(name = None, org = None, repo = None, version = "", ref = "master", first_srcs = [], deps = [], **kwargs):
    if not ("build_file" in kwargs.keys() or "build_file_content" in kwargs.keys()):
        kwargs.update(build_file_content = _BUILD_FILE_TEMPLATE.format(
            app_name = name,
            version = version,
            first_srcs = first_srcs,
            deps = deps,
        ))

    repo = name if repo == None else repo

    http_archive(
        name = name,
        urls = ["https://github.com/{}/{}/archive/{}.zip".format(org, repo, ref)],
        strip_prefix = "{}-{}".format(repo, ref),
        **kwargs
    )

_BUILD_FILE_TEMPLATE = """
load("@bazel-erlang//:bazel_erlang_lib.bzl", "erlang_lib")

erlang_lib(
    app_name = "{app_name}",
    app_version = "{version}",
    first_srcs = {first_srcs},
    deps = {deps},
)
"""
