#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname dot_app_to_json -nocookie
-mode(compile).

-export([main/1]).

main(Args) ->
    case io:get_line("") of
        eof ->
            ok;
        Line ->
            Filename = parse_json_string(Line),
            Json = parse(Filename),
            io:format("~s", [Json]),
            % signal to app_file_parser.go that the json is written
            io:format(<<0>>),
            main(Args)
    end.

parse_json_string(Line) ->
    case string:trim(Line, trailing, "\n") of
        "\"" ++ Tail ->
            case string:reverse(Tail) of
                "\"" ++ Middle ->
                    string:reverse(Middle)
            end
    end.

map_keys(F, M) ->
    maps:from_list(
      lists:map(
        fun ({K, V}) -> {F(K), V} end,
        maps:to_list(M))).

atom_to_list_binary(A) ->
    list_to_binary(atom_to_list(A)).

conform(List) ->
    map_keys(
      fun atom_to_list_binary/1,
      maps:map(
        fun
            (description, Desc) ->
                list_to_binary(Desc);
            (vsn, Vsn) ->
                list_to_binary(Vsn);
            (licenses, Licenses) ->
                lists:map(fun list_to_binary/1, Licenses);
            (applications, Apps) ->
                lists:map(fun atom_to_list_binary/1, Apps);
            (_, _) ->
                <<"ignored">>
        end,
        proplists:to_map(List))).

parse(MetadataFile) ->
    {ok, Metadata} = file:consult(MetadataFile),
    [{application, App, Props}] = Metadata,
    Map = #{atom_to_list_binary(App) => conform(Props)},
    % io:format(standard_error, "Map: ~p~n", [Map]),
    to_json(Map).

to_json(M) when is_map(M) ->
    Pairs = [to_json(K) ++ ": " ++ to_json(V) || {K, V} <- maps:to_list(M)],
    "{" ++ string:join(Pairs, ",") ++ "}";
to_json(S) when is_binary(S) ->
    "\"" ++ binary_to_list(S) ++ "\"";
to_json(A) when is_atom(A) ->
    atom_to_list(A);
to_json(L) when is_list(L) ->
    Items = lists:map(fun to_json/1, L),
    "[" ++ string:join(Items, ",") ++ "]".
