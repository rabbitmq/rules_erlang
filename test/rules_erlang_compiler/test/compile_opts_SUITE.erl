-module(compile_opts_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          transform_erlc_opts
         ].

transform_erlc_opts(_) ->
    ?assertEqual([warnings_as_errors,deterministic,debug_info,
                  warn_export_vars,warn_shadow_vars,
                  warn_obsolete_guard,
                  {d, namespaced_dicts}],
                 compile_opts:transform_erlc_opts([
                                                   "-Werror",
                                                   "+deterministic",
                                                   "+debug_info",
                                                   "+warn_export_vars",
                                                   "+warn_shadow_vars",
                                                   "+warn_obsolete_guard",
                                                   "-Dnamespaced_dicts"
                                                  ])),
    ?assertEqual([warnings_as_errors,deterministic,
                  {d, namespaced_dicts},
                  {d, 'TEST', 1}],
                 compile_opts:transform_erlc_opts([
                                                   "-Werror",
                                                   "+deterministic",
                                                   "-Dnamespaced_dicts",
                                                   "'-DTEST=1'"
                                                  ])).
