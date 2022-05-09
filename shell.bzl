load(
    "//private:shell.bzl",
    _shell = "shell",
)
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_INSTALLATION",
)

def shell(
        erlang_installation = DEFAULT_ERLANG_INSTALLATION,
        **kwargs):
    _shell(
        erlang_installation = erlang_installation,
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
