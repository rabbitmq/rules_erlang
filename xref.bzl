load("//private:xref.bzl", "xref_test")
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_INSTALLATION",
)

def xref(
        erlang_installation = DEFAULT_ERLANG_INSTALLATION,
        **kwargs):
    xref_test(
        name = "xref",
        erlang_installation = erlang_installation,
        target = ":erlang_app",
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
