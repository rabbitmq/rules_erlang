load("//private:xref.bzl", "xref_test")
load("//tools:erlang.bzl", "DEFAULT_LABEL")

def xref(
        erlang_version_label = DEFAULT_LABEL,
        **kwargs):
    xref_test(
        name = "xref",
        erlang_installation = Label("//tools:otp-{}-installation".format(erlang_version_label)),
        target = ":erlang_app",
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
