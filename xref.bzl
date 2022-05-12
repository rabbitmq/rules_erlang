load("//private:xref.bzl", "xref_test")
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_INSTALLATION",
    "installation_suffix",
)

def xref(
        erlang_installations = [DEFAULT_ERLANG_INSTALLATION],
        tags = [],
        **kwargs):
    for erlang_installation in erlang_installations:
        suffix = installation_suffix(erlang_installation)
        xref_test(
            name = "xref-{}".format(suffix),
            erlang_installation = erlang_installation,
            target = ":erlang_app-{}".format(suffix),
            is_windows = select({
                "@bazel_tools//src/conditions:host_windows": True,
                "//conditions:default": False,
            }),
            tags = tags + [suffix],
            **kwargs
        )
