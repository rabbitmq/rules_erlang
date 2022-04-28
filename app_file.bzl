load(
    "//private:app_file.bzl",
    _app_file = "app_file",
)
load("//tools:erlang.bzl", "DEFAULT_LABEL")

def app_file(
        erlang_version_label = DEFAULT_LABEL,
        **kwargs):
    _app_file(
        erlang_installation = Label("//tools:otp-{}-installation".format(erlang_version_label)),
        app_file_tool = Label("//tools/app_file_tool:escript-{}".format(erlang_version_label)),
        private_stamp_detect = select({
            Label("//private:private_stamp_detect"): True,
            "//conditions:default": False,
        }),
        **kwargs
    )
