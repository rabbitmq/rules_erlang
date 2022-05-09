load(
    "//private:dialyze.bzl",
    "dialyze_test",
)
load(
    "//private:plt.bzl",
    _DEFAULT_PLT_APPS = "DEFAULT_PLT_APPS",
    _plt = "plt",
)
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_INSTALLATION",
)

DEFAULT_PLT_APPS = _DEFAULT_PLT_APPS

def plt(
        erlang_installation = DEFAULT_ERLANG_INSTALLATION,
        **kwargs):
    _plt(
        erlang_installation = erlang_installation,
        **kwargs
    )

def dialyze(
        erlang_installation = DEFAULT_ERLANG_INSTALLATION,
        **kwargs):
    dialyze_test(
        name = "dialyze",
        erlang_installation = erlang_installation,
        target = ":erlang_app",
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
