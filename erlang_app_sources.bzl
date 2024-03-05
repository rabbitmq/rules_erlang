load(
    "//private:erlang_app_sources.bzl",
    _erlang_app_sources = "erlang_app_sources",
)

def erlang_app_sources(
        app_name = None,
        app_src = None,
        public_hdrs = None,
        private_hdrs = None,
        srcs = None,
        priv = None,
        license_files = None,
        **kwargs):

    app_src_paths = native.glob(["src/%s.app.src" % app_name])
    if len(app_src_paths) == 1:
        app_src = app_src_paths[0]

    if public_hdrs == None:
        public_hdrs = native.glob([
            "include/**/*.hrl",
        ])

    if private_hdrs == None:
        private_hdrs = native.glob([
            "src/**/*.hrl",
        ])

    if srcs == None:
        srcs = native.glob([
            "src/**/*.erl",
        ])

    if priv == None:
        priv = native.glob(
            ["priv/**/*"],
            exclude = ["priv/**/.gitignore"],
        )

    if license_files == None:
        license_files = native.glob([
            "LICENSE*",
        ])

    _erlang_app_sources(
        app_name = app_name,
        app_src = app_src,
        public_hdrs = public_hdrs,
        private_hdrs = private_hdrs,
        srcs = srcs,
        priv = priv,
        license_files = license_files,
        **kwargs
    )
