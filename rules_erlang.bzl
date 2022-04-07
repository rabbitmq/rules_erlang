load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")

def rules_erlang_dependencies():
    http_file(
        name = "xrefr",
        urls = ["https://github.com/inaka/xref_runner/releases/download/1.2.0/xrefr"],
        executable = True,
        sha256 = "b50df06b8112852722667b5a5afdb0a9f4549cd5da7a0c157cfd34f29eee3b5c",
    )

    http_archive(
        name = "jsx-source",
        urls = ["https://github.com/talentdeficit/jsx/archive/refs/tags/v3.1.0.zip"],
        strip_prefix = "jsx-3.1.0",
        build_file_content = """
exports_files(["src/jsx.app.src"])

filegroup(
    name = "hdrs",
    srcs = glob(["include/**/*.hrl", "src/**/*.hrl"]),
    visibility = ["//visibility:public"],
)

filegroup(
    name = "srcs",
    srcs = glob(["src/**/*.erl"]),
    visibility = ["//visibility:public"],
)

filegroup(
    name = "priv",
    srcs = glob(["priv/**/*"]),
    visibility = ["//visibility:public"],
)
""",
    )
