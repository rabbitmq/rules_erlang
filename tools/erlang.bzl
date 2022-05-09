load(
    "//private:erlang_installation.bzl",
    "erlang_installation",
)
load(
    "//private:erlang_tool.bzl",
    "erlang_tool",
)

DEFAULT_VERSION = "24.3.3"  # <- must match MODULE.bazel
DEFAULT_ERLANG_INSTALLATION = "@otp_{}//:otp".format(DEFAULT_VERSION)

def standard_erlang_tools(name_suffix = ""):
    erlang_installation(
        name = "otp{}".format(name_suffix),
        sources = native.glob(
            ["**/*"],
            exclude = ["BUILD.bazel", "WORKSPACE.bazel"],
        ),
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
        name = "erl{}".format(name_suffix),
        erlang_installation = ":otp{}".format(name_suffix),
        path = "bin/erl",
        visibility = ["//visibility:public"],
    )

    erlang_tool(
        name = "erlc{}".format(name_suffix),
        erlang_installation = ":otp{}".format(name_suffix),
        path = "bin/erlc",
        visibility = ["//visibility:public"],
    )

    erlang_tool(
        name = "escript{}".format(name_suffix),
        erlang_installation = ":otp{}".format(name_suffix),
        path = "bin/escript",
        visibility = ["//visibility:public"],
    )
