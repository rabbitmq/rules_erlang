load(":hex_archive.bzl", "hex_archive")
load("//bzlmod:erlang_package.bzl", "DEFAULT_BUILD_FILE_CONTENT")

def hex_pm_erlang_app(
        name = None,
        testonly = False,
        app_name = None,
        **kwargs):
    if not ("build_file" in kwargs.keys() or "build_file_content" in kwargs.keys()):
        if app_name == None:
            app_name = name
        kwargs.update(build_file_content = DEFAULT_BUILD_FILE_CONTENT.format(
            app_name = app_name,
            testonly = testonly,
        ))

    hex_archive(
        name = name,
        version = version,
        **kwargs
    )
