load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("//bzlmod:erlang_package.bzl", "DEFAULT_BUILD_FILE_CONTENT")

def github_erlang_app(
        name = None,
        org = None,
        repo = None,
        version = "master",
        ref = "refs/heads/master",
        strip_prefix = None,
        testonly = False,
        **kwargs):
    if not ("build_file" in kwargs.keys() or "build_file_content" in kwargs.keys()):
        kwargs.update(build_file_content = DEFAULT_BUILD_FILE_CONTENT.format(
            app_name = name,
            testonly = testonly,
        ))

    repo = name if repo == None else repo
    strip_prefix = "{}-{}".format(repo, version) if strip_prefix == None else strip_prefix

    http_archive(
        name = name,
        urls = ["https://github.com/{}/{}/archive/{}.zip".format(org, repo, ref)],
        strip_prefix = strip_prefix,
        **kwargs
    )
