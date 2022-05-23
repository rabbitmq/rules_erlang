load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)
load(
    "//bzlmod:otp.bzl",
    "OTP_BUILD_FILE_CONTENT",
)
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_SHA256",
    "DEFAULT_ERLANG_VERSION",
)
load(
    ":hex_archive.bzl",
    "hex_archive",
)

def rules_erlang_dependencies():
    xref_runner_sources()

def xref_runner_sources():
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

def otp_default(rules_erlang_workspace = "@rules_erlang"):
    otp_github_release(
        name = "otp_default",
        version = DEFAULT_ERLANG_VERSION,
        sha256 = DEFAULT_ERLANG_SHA256,
        rules_erlang_workspace = rules_erlang_workspace,
    )

def otp_github_release(
        name = None,
        version = None,
        sha256 = None,
        rules_erlang_workspace = "@rules_erlang"):
    http_archive(
        name = name,
        url = "https://github.com/erlang/otp/releases/download/OTP-{v}/otp_src_{v}.tar.gz".format(v = version),
        strip_prefix = "otp_src_{}".format(version),
        sha256 = sha256,
        build_file_content = OTP_BUILD_FILE_CONTENT,
        repo_mapping = {
            "@rules_erlang": rules_erlang_workspace,
        },
    )
