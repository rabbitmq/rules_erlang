load(
    "//private:app_file.bzl",
    _app_file = "app_file",
)
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_INSTALLATION",
)

def app_file(
        erlang_installation = DEFAULT_ERLANG_INSTALLATION,
        **kwargs):
    _app_file(
        erlang_installation = erlang_installation,
        private_stamp_detect = select({
            Label("//private:private_stamp_detect"): True,
            "//conditions:default": False,
        }),
        **kwargs
    )
