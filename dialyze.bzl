load("//private:dialyze.bzl", "dialyze_test")
load(
    "//private:plt.bzl",
    _DEFAULT_PLT_APPS = "DEFAULT_PLT_APPS",
    _plt = "plt",
)

DEFAULT_PLT_APPS = _DEFAULT_PLT_APPS

def plt(**kwargs):
    _plt(**kwargs)

def dialyze(**kwargs):
    dialyze_test(
        name = "dialyze",
        target = ":erlang_app",
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
