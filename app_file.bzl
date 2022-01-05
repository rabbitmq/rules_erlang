load("//private:app_file.bzl", "app_file_private")

def app_file(**kwargs):
    app_file_private(
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
