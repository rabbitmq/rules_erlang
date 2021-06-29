load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_file")

def bazel_erlang_deps():
    http_file(
        name = "xrefr",
        urls = ["https://github.com/inaka/xref_runner/releases/download/1.2.0/xrefr"],
        executable = True,
        sha256 = "b50df06b8112852722667b5a5afdb0a9f4549cd5da7a0c157cfd34f29eee3b5c",
    )
