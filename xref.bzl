load("//private:xref.bzl", "xref_test")

def xref(**kwargs):
    xref_test(
        name = "xref",
        target = ":erlang_app",
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
