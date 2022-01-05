load("//private:erlc.bzl", "erlc_private")

def erlc(**kwargs):
    erlc_private(
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
