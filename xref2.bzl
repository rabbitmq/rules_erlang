load(
    "//private:xref2.bzl",
    "xref_query",
    "xref_test",
)
load(
    ":xref.bzl",
    _XREF_TAG = "XREF_TAG",
)

XREF_TAG = _XREF_TAG

def xref(
        name = "xref",
        target = ":erlang_app",
        tags = [],
        **kwargs):
    xref_test(
        name = name,
        target = target,
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        tags = tags + [XREF_TAG],
        **kwargs
    )
    xref_query(
        name = name + "-query",
        target = target,
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        tags = tags,
        **kwargs
    )
