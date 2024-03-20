load(
    "//private:extract_app.bzl",
    _extract_app = "extract_app",
)
load(
    "//private:extract_app_test.bzl",
    "extract_app_test",
)

def extract_app(
        name = None,
        verify = True,
        **kwargs):
    _extract_app(
        name = name,
        **kwargs
    )

    if verify:
        extract_app_test(
            name = "%s_assert_dot_app" % name,
            app = ":%s" % name,
            is_windows = select({
                "@bazel_tools//src/conditions:host_windows": True,
                "//conditions:default": False,
            }),
            assert_applications = Label("@rules_erlang//tools/assert_applications:assert_applications"),
        )
