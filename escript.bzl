load(
    "//private:escript_archive.bzl",
    _escript_archive = "escript_archive",
)
load(
    "//private:escript_flat.bzl",
    _escript_flat = "escript_flat",
)
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_INSTALLATION",
)

def escript_archive(
        erlang_installation = DEFAULT_ERLANG_INSTALLATION,
        **kwargs):
    _escript_archive(
        erlang_installation = erlang_installation,
        **kwargs
    )

def escript_flat(
        erlang_installation = DEFAULT_ERLANG_INSTALLATION,
        **kwargs):
    _escript_flat(
        erlang_installation = erlang_installation,
        **kwargs
    )
