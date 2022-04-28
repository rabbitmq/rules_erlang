load(
    "//private:erlang_installation.bzl",
    _erlang_installation = "erlang_installation",
)
load(
    "//private:erlang_tool.bzl",
    _erlang_tool = "erlang_tool",
)

def erlang_installation(**kwargs):
    _erlang_installation(**kwargs)

def erlang_tool(**kwargs):
    _erlang_tool(**kwargs)

DEFAULT_LABEL = "default-24"
DEFAULT_VERSION = "24.3.3"

def standard_erlang_tools(
        label = DEFAULT_LABEL,
        version = DEFAULT_VERSION):
    erlang_installation(
        name = "otp-{}-installation".format(label),
        sources = ["@otp_src_{}//:all".format(version)],
        extra_env = select({
            "@bazel_tools//src/conditions:darwin": {
                "CC": "clang",
            },
            "//conditions:default": {},
        }),
        extra_configure_opts = select({
            "@bazel_tools//src/conditions:darwin": [
                "--enable-darwin-64bit",
                # "--with-ssl=$(brew --prefix openssl@1.1)",
            ],
            "//conditions:default": [],
        }),
        visibility = ["//visibility:public"],
    )

    erlang_tool(
        name = "erl-{}".format(label),
        erlang_installation = ":otp-{}-installation".format(label),
        path = "bin/erl",
        visibility = ["//visibility:public"],
    )

    erlang_tool(
        name = "erlc-{}".format(label),
        erlang_installation = ":otp-{}-installation".format(label),
        path = "bin/erlc",
        visibility = ["//visibility:public"],
    )

    erlang_tool(
        name = "escript-{}".format(label),
        erlang_installation = ":otp-{}-installation".format(label),
        path = "bin/escript",
        visibility = ["//visibility:public"],
    )
