load("//private:erlang_bytecode.bzl", "erlang_bytecode")
load("//private:escript_flat.bzl", "escript_flat")
load("//tools:erlang.bzl", "DEFAULT_VERSION")

DEFAULT_APP_FILE_TOOL = "@otp_{}//:app_file_tool".format(DEFAULT_VERSION)

def app_file_tool(name_suffix = ""):
    erlang_bytecode(
        name = "app_file_tool_beam{}".format(name_suffix),
        erlang_installation = ":otp{}".format(name_suffix),
        srcs = [
            Label("//tools/app_file_tool:src/app_file_tool.erl"),
        ],
    )

    escript_flat(
        name = "app_file_tool{}".format(name_suffix),
        erlang_installation = ":otp{}".format(name_suffix),
        beam = ["app_file_tool_beam{}".format(name_suffix)],
        visibility = ["//visibility:public"],
    )
