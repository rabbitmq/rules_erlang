load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_file")

def bazel_erlang_deps():
    http_file(
        name = "xrefr",
        urls = ["https://github.com/inaka/xref_runner/releases/download/1.1.0/xrefr"],
        executable = True,
        sha256 = "d2b5872237356ae2a7f824414bce0e334e8ef270763fb8d0e7e0f76b7eaf971f",
    )
