-module(digraph_tools_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() ->
    [
        consume_to_list
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

    ?assertEqual(
        [Root, Middle, Leaf1, Leaf2, Loner],
        digraph_tools:consume_to_list(G1)
    ).
