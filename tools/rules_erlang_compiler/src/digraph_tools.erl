-module(digraph_tools).

-export([consume_to_list/1]).

-spec consume_to_list(digraph:graph()) -> [digraph:vertex()].
consume_to_list(G) ->
    consume_to_list(G, []).

consume_to_list(G, Acc) ->
    case lists:sort(digraph:vertices(G)) of
        [] ->
            digraph:delete(G),
            Acc;
        [V | _] ->
            R = find_root(G, V),
            digraph:del_vertex(G, R),
            consume_to_list(G, Acc ++ [R])
    end.

find_root(G, V) ->
    Edges = lists:sort(
        fun(E1, E2) ->
            {E1, P1, V, _} = digraph:edge(G, E1),
            {E2, P2, V, _} = digraph:edge(G, E2),
            case lists:sort([P1, P2]) of
                [P1, P2] -> true;
                [P2, P1] -> false
            end
        end,
        digraph:in_edges(G, V)
    ),
    case Edges of
        [] ->
            V;
        [E | _] ->
            {E, P, V, _} = digraph:edge(G, E),
            find_root(G, P)
    end.
