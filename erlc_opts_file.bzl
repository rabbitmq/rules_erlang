load(
    "//private:erlc_opts_file.bzl",
    _erlc_opts_file = "erlc_opts_file",
)

def erlc_opts_file(
        out = "erlc_opts_file",
        **kwargs):
    _erlc_opts_file(
        out = out,
        **kwargs
    )
