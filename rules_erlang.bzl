load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")

def rules_erlang_dependencies(
        erlang_version = "24.3.3",
        erlang_sha256 = "cc3177f765c6a2b018e9a80c30bd3eac9a1f1d4c2690bb10557b384a9a63ae8d"):
    http_file(
        name = "xrefr",
        urls = ["https://github.com/inaka/xref_runner/releases/download/1.2.0/xrefr"],
        executable = True,
        sha256 = "b50df06b8112852722667b5a5afdb0a9f4549cd5da7a0c157cfd34f29eee3b5c",
    )

    http_archive(
        name = "otp_src_{}".format(erlang_version),
        url = "https://github.com/erlang/otp/releases/download/OTP-{v}/otp_src_{v}.tar.gz".format(v = erlang_version),
        strip_prefix = "otp_src_{}".format(erlang_version),
        sha256 = erlang_sha256,
        build_file_content = """filegroup(
    name = "all",
    srcs = glob(["**/*"], exclude = ["BUILD.bazel", "WORKSPACE.bazel"]),
    visibility = ["//visibility:public"],
)
""",
    )
