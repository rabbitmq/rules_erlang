load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load(":hex_archive.bzl", "hex_archive")

def rules_erlang_internal_deps():
    maybe(
        http_archive,
        name = "bazel_skylib",
        sha256 = "66ffd9315665bfaafc96b52278f57c7e2dd09f5ede279ea6d39b2be471e7e3aa",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.4.2/bazel-skylib-1.4.2.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.4.1/bazel-skylib-1.4.2.tar.gz",
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
    hex_archive(
        name = "thoas_rules_erlang",
        package_name = "thoas",
        version = "1.0.0",
        sha256 = "fc763185b932ecb32a554fb735ee03c3b6b1b31366077a2427d2a97f3bd26735",
        build_file = "@rules_erlang//:BUILD.thoas",
    )
