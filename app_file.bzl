load("//private:app_file.bzl", "app_file_private")

_stamp_condition = str(Label("//private:private_stamp_detect"))

def app_file(**kwargs):
    app_file_private(
        private_stamp_detect = select({
            _stamp_condition: True,
            "//conditions:default": False,
        }),
        **kwargs
    )
