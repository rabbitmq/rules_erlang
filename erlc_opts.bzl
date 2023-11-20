load(
    "//private:erlc_opts.bzl",
    _ErlcOptsInfo = "ErlcOptsInfo",
    _erlc_opts = "erlc_opts",
)

ErlcOptsInfo = _ErlcOptsInfo

def erlc_opts(**kwargs):
    _erlc_opts(**kwargs)
