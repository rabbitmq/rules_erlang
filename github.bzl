load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def github_bazel_erlang_lib(name = None, org = None, repo = None, version = None, tag = None, sha256 = None, first_srcs = [], deps = [], **kwargs):
    if not ("build_file" in kwargs.keys() or "build_file_content" in kwargs.keys()):
        kwargs.update(build_file_content = _BUILD_FILE_TEMPLATE.format(
            app_name = name,
            version = version,
            first_srcs = first_srcs,
            deps = deps,
        ))

    tag = "v{}".format(version) if tag == None else tag

    repo = name if repo == None else repo

    http_archive(
        name = name,
        urls = ["https://github.com/{}/{}/archive/{}.zip".format(org, repo, tag)],
        sha256 = sha256,
        strip_prefix = "{}-{}".format(repo, version),
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
