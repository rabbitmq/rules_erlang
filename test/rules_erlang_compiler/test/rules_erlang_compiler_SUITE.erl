-module(rules_erlang_compiler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          consume_to_list,
          transform_erlc_opts
         ].

consume_to_list(_) ->
    G1 = digraph:new([acyclic]),

    Root = digraph:add_vertex(G1, "root"),
    Middle = digraph:add_vertex(G1, "middle"),
    Leaf1 = digraph:add_vertex(G1, "leaf1"),
    Leaf2 = digraph:add_vertex(G1, "leaf2"),
    Loner = digraph:add_vertex(G1, "loner"),

    digraph:add_edge(G1, Root, Middle),
    digraph:add_edge(G1, Middle, Leaf1),
    digraph:add_edge(G1, Middle, Leaf2),

    ?assertEqual([Root, Middle, Leaf1, Leaf2, Loner],
                 rules_erlang_compiler:consume_to_list(G1)).

transform_erlc_opts(_) ->
    ?assertEqual([warnings_as_errors,deterministic,debug_info,
                  warn_export_vars,warn_shadow_vars,
                  warn_obsolete_guard,
                  {d, namespaced_dicts}],
                 rules_erlang_compiler:transform_erlc_opts([
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
                 rules_erlang_compiler:transform_erlc_opts([
                                                            "-Werror",
                                                            "+deterministic",
                                                            "-Dnamespaced_dicts",
                                                            "-DTEST=1"
                                                           ])).

