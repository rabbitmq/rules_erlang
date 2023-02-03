load("//private:eunit.bzl", "eunit_test")

def eunit(
        name = "eunit",
        **kwargs):
    eunit_test(
        name = name,
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
