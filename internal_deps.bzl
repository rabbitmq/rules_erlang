load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load(":hex_archive.bzl", "hex_archive")

def rules_erlang_internal_deps():
    maybe(
        http_archive,
        name = "rules_pkg",
        sha256 = "a89e203d3cf264e564fcb96b6e06dd70bc0557356eb48400ce4b5d97c2c3720d",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/rules_pkg/releases/download/0.5.1/rules_pkg-0.5.1.tar.gz",
            "https://github.com/bazelbuild/rules_pkg/releases/download/0.5.1/rules_pkg-0.5.1.tar.gz",
        ],
    )

    maybe(
        http_archive,
        name = "bazel_skylib",
        sha256 = "74d544d96f4a5bb630d465ca8bbcfe231e3594e5aae57e1edbf17a6eb3ca2506",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.3.0/bazel-skylib-1.3.0.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.3.0/bazel-skylib-1.3.0.tar.gz",
        ],
    )

    maybe(
        http_archive,
        name = "io_bazel_rules_go",
        sha256 = "56d8c5a5c91e1af73eca71a6fab2ced959b67c86d12ba37feedb0a2dfea441a6",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.37.0/rules_go-v0.37.0.zip",
            "https://github.com/bazelbuild/rules_go/releases/download/v0.37.0/rules_go-v0.37.0.zip",
        ],
    )

    maybe(
        http_archive,
        name = "bazel_gazelle",
        sha256 = "448e37e0dbf61d6fa8f00aaa12d191745e14f07c31cabfa731f0c8e8a4f41b97",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.28.0/bazel-gazelle-v0.28.0.tar.gz",
            "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.28.0/bazel-gazelle-v0.28.0.tar.gz",
        ],
    )

    hex_archive(
        name = "getopt_src",
        package_name = "getopt",
        version = "1.0.1",
        sha256 = "53e1ab83b9ceb65c9672d3e7a35b8092e9bdc9b3ee80721471a161c10c59959c",
        build_file_content = """filegroup(
    name = "app_src",
    srcs = glob(["src/*.app.src"]),
    visibility = ["//visibility:public"],
)
filegroup(
    name = "srcs",
    srcs = glob(["src/**/*.erl"]),
    visibility = ["//visibility:public"],
)
""",
    )
    hex_archive(
        name = "xref_runner_src",
        package_name = "xref_runner",
        version = "1.2.0",
        sha256 = "22d4bb466b1bf8b206f03d1f43f01233b547f8b81351f29af2c6d668e0734ffc",
        build_file_content = """filegroup(
    name = "app_src",
    srcs = glob(["src/*.app.src"]),
    visibility = ["//visibility:public"],
)
filegroup(
    name = "srcs",
    srcs = glob(["src/**/*.erl"]),
    visibility = ["//visibility:public"],
)
""",
    )
