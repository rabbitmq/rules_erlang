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
    "installation_suffix",
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
        erlang_installations = [DEFAULT_ERLANG_INSTALLATION],
        **kwargs):
    for erlang_installation in erlang_installations:
        suffix = installation_suffix(erlang_installation)
        dialyze_test(
            name = "dialyze-{}".format(suffix),
            erlang_installation = erlang_installation,
            target = ":erlang_app-{}".format(suffix),
            is_windows = select({
                "@bazel_tools//src/conditions:host_windows": True,
                "//conditions:default": False,
            }),
            **kwargs
        )
