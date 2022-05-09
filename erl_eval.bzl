load(
    "//private:erl_eval.bzl",
    _erl_eval = "erl_eval",
)
load(
    "//tools:erlang.bzl",
    "DEFAULT_ERLANG_INSTALLATION",
)

def erl_eval(
        erlang_installation = DEFAULT_ERLANG_INSTALLATION,
        **kwargs):
    _erl_eval(
        erlang_installation = erlang_installation,
        **kwargs
    )
