load("//private:app_file.bzl", "app_file_private")

_stamp_condition = str(Label("//private:private_stamp_detect"))

def app_file(
        app_extra_keys = [],
        app_extra = "",
        **kwargs):
    if len(app_extra_keys) > 0 and app_extra != "":
        all_app_extra = ",".join(app_extra_keys + [app_extra])
    elif len(app_extra_keys) > 0:
        all_app_extra = ",".join(app_extra_keys)
    else:
        all_app_extra = app_extra

    app_file_private(
        private_stamp_detect = select({
            _stamp_condition: True,
            "//conditions:default": False,
        }),
        app_extra_keys = all_app_extra,
        **kwargs
    )
