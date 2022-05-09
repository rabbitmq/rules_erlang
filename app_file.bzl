load(
    "//private:app_file.bzl",
    _app_file = "app_file",
)
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_INSTALLATION",
)
load(
    "//tools/app_file_tool:app_file_tool.bzl",
    "DEFAULT_APP_FILE_TOOL",
)

def app_file(
        erlang_installation = DEFAULT_ERLANG_INSTALLATION,
        app_file_tool = DEFAULT_APP_FILE_TOOL,
        **kwargs):
    _app_file(
        erlang_installation = erlang_installation,
        app_file_tool = app_file_tool,
        private_stamp_detect = select({
            Label("//private:private_stamp_detect"): True,
            "//conditions:default": False,
        }),
        **kwargs
    )
