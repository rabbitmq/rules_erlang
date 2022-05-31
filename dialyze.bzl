load(
    "//private:dialyze.bzl",
    "dialyze_test",
)
load(
    "//private:plt.bzl",
    _DEFAULT_PLT_APPS = "DEFAULT_PLT_APPS",
    _plt = "plt",
)

DEFAULT_PLT_APPS = _DEFAULT_PLT_APPS

DIALYZE_TAG = "dialyze"

def plt(**kwargs):
    _plt(**kwargs)

def dialyze(
        name = "dialyze",
        target = ":erlang_app",
        tags = [],
        **kwargs):
    dialyze_test(
        name = name,
        target = target,
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        tags = tags + [DIALYZE_TAG],
        **kwargs
    )
