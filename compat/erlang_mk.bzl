load(
    "//compat/private:app_src_from_erlang_mk_makefile.bzl",
    _app_src_from_erlang_mk_makefile = "app_src_from_erlang_mk_makefile",
)
load(
    "//compat/private:erlc_opts_from_erlang_mk_makefile.bzl",
    _erlc_opts_from_erlang_mk_makefile = "erlc_opts_from_erlang_mk_makefile",
)

def app_src_from_erlang_mk_makefile(**kwargs):
    _app_src_from_erlang_mk_makefile(**kwargs)

def erlc_opts_from_erlang_mk_makefile(**kwargs):
    _erlc_opts_from_erlang_mk_makefile(**kwargs)
