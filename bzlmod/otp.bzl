OTP_BUILD_FILE_CONTENT = """load(
    "@rules_erlang//tools:erlang.bzl",
    "standard_erlang_tools",
)

standard_erlang_tools(major_version = {major_version})
"""

OTP_PATCH_GETOPT_DIR = """mkdir -p bazel/getopt
cat << EOF > bazel/getopt/BUILD.bazel
load("@rules_erlang//:app_file.bzl", "app_file")
load("@rules_erlang//:erlang_bytecode.bzl", "erlang_bytecode")
load("@rules_erlang//:erlang_app_info.bzl", "erlang_app_info")

APP_NAME = "getopt"

erlang_bytecode(
    name = "beam_files",
    erlang_installation = "//:erlang_installation_compilation",
    srcs = ["@getopt_src//:srcs"],
)
app_file(
    name = "app_file",
    erlang_installation = "//:erlang_installation_compilation",
    app_name = APP_NAME,
    app_src = ["@getopt_src//:app_src"],
    stamp = 0,
)
erlang_app_info(
    name = "erlang_app",
    app_name = APP_NAME,
    app = ":app_file",
    beam = [":beam_files"],
    visibility = ["//bazel/xref_runner:__pkg__"],
)
EOF
"""

OTP_PATCH_XREF_RUNNER_DIR = """mkdir -p bazel/xref_runner
cat << EOF > bazel/xref_runner/BUILD.bazel
load("@rules_erlang//:app_file.bzl", "app_file")
load("@rules_erlang//:erlang_bytecode.bzl", "erlang_bytecode")
load("@rules_erlang//:erlang_app_info.bzl", "erlang_app_info")
load("@rules_erlang//:escript.bzl", "escript_archive")

APP_NAME = "xref_runner"

erlang_bytecode(
    name = "beam_files",
    erlang_installation = "//:erlang_installation_compilation",
    srcs = ["@xref_runner_src//:srcs"],
)
app_file(
    name = "app_file",
    erlang_installation = "//:erlang_installation_compilation",
    app_name = APP_NAME,
    app_src = ["@xref_runner_src//:app_src"],
    stamp = 0,
)
erlang_app_info(
    name = "erlang_app",
    app_name = APP_NAME,
    app = ":app_file",
    beam = [":beam_files"],
    deps = [
        "//bazel/getopt:erlang_app",
    ],
)

escript_archive(
    name = "xrefr",
    erlang_installation = "//:erlang_installation_compilation",
    app = ":erlang_app",
    visibility = ["//visibility:public"],
)
EOF
"""

def merge_archive(an_archive, archives):
    for archive in archives:
        if archive["url"] == an_archive["url"]:
            if archive == an_archive:
                return archives
            else:
                fail("Conflicting definitions for otp src: {}, {}".format(an_archive, archive))
    archives.append(an_archive)
    return archives
