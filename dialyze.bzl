load("//private:dialyze.bzl", "dialyze_test")
load(
    "//private:plt.bzl",
    _DEFAULT_PLT_APPS = "DEFAULT_PLT_APPS",
    _plt = "plt",
)
load("//tools:erlang.bzl", "DEFAULT_LABEL")

DEFAULT_PLT_APPS = _DEFAULT_PLT_APPS

def plt(
        erlang_version_label = DEFAULT_LABEL,
        **kwargs):
    _plt(
        erlang_installation = Label("//tools:otp-{}-installation".format(erlang_version_label)),
        **kwargs
    )

def dialyze(
        erlang_version_label = DEFAULT_LABEL,
        **kwargs):
    dialyze_test(
        name = "dialyze",
        erlang_installation = Label("//tools:otp-{}-installation".format(erlang_version_label)),
        target = ":erlang_app",
        is_windows = select({
            "@bazel_tools//src/conditions:host_windows": True,
            "//conditions:default": False,
        }),
        **kwargs
    )
