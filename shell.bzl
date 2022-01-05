load("//private:shell.bzl", "shell_private")

def shell(**kwargs):
    shell_private(
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
