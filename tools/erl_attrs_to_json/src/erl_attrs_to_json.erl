-module(erl_attrs_to_json).

-include("erl_attrs_to_json.hrl").

-export([main/1]).

-ifdef(TEST).
-export([conform_command/1]).
-endif.

-spec main([string()]) -> no_return().
main(Args) ->
    case io:get_line("") of
        eof ->
            ok;
        Line ->
            {ok, Command} = thoas:decode(Line),
            %% io:format(standard_error, "Command: ~p~n", [Command]),
            #{path := Filename,
              macros := Macros,
              includes := Includes} = conform_command(Command),
            Map = parser:parse(Filename, Macros, Includes),
            %% io:format(standard_error, "Map: ~p~n", [Map]),
            Json = thoas:encode(Map),
            io:format("~ts", [Json]),
            % signal to erl_parser_impl.go that the json is written
            io:format(<<0>>),
            main(Args)
    end.

-spec conform_command(thoas:json_term()) -> #{path := string(), macros := [macro()], includes := [string()]}.
conform_command(JsonTerm) when is_map(JsonTerm) ->
    maps:from_list(
     lists:map(fun
                   ({<<"path">> = K, V}) ->
                       {binary_to_atom(K), binary_to_list(V)};
                   ({<<"macros">> = K, V}) ->
                       {binary_to_atom(K), conform_macros(V)};
                   ({<<"includes">> = K, V}) ->
                       {binary_to_atom(K), lists:map(fun erlang:binary_to_list/1, V)}
               end, maps:to_list(JsonTerm))).

conform_macros(M) when is_map(M) ->
    lists:map(fun
                  ({K, null}) ->
                      binary_to_atom(K);
                  ({K, V}) ->
                      {binary_to_atom(K), binary_to_list(V)}
              end, maps:to_list(M)).
