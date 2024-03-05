load(
    "//private:escript_archive.bzl",
    _escript_archive = "escript_archive",
)
load(
    "//private:escript_flat.bzl",
    _escript_flat = "escript_flat",
)
load(
    "//private:escript_wrapper.bzl",
    _escript_wrapper = "escript_wrapper",
)

def escript_archive(**kwargs):
    return _escript_archive(**kwargs)

def escript_flat(**kwargs):
    return _escript_flat(**kwargs)

def escript_wrapper(**kwargs):
    return _escript_wrapper(**kwargs)
