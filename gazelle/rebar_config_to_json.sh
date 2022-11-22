#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname rebar_config_to_json -nocookie
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
            % signal to hex_metadata.go that the json is written
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

mapify_dep({Name, _, {git = Kind, Remote, Ref}}) ->
    #{name => Name,
      kind => Kind,
      remote => Remote,
      ref => Ref};
mapify_dep({Name, Version}) ->
    #{name => Name,
      kind => hex,
      version => Version}.

conform(List) ->
    maps:map(
        fun
            (erl_opts, Opts) ->
                Opts;
            (deps, Deps) ->
                [mapify_dep(D) || D <- Deps];
            (_, _) ->
                ignored
        end,
        proplists:to_map(List)).

parse(MetadataFile) ->
    {ok, Metadata} = file:consult(MetadataFile),
    Map = conform(Metadata),
    %% io:format(standard_error, "Map: ~p~n", [Map]),
    to_json(Map).

to_json(M) when is_map(M) ->
    Pairs = [to_json(K) ++ ": " ++ to_json(V) || {K, V} <- maps:to_list(M)],
    "{" ++ string:join(Pairs, ",") ++ "}";
to_json(S) when is_binary(S) ->
    "\"" ++ binary_to_list(S) ++ "\"";
to_json(A) when is_atom(A) ->
    "\"" ++ atom_to_list(A) ++ "\"";
to_json([]) ->
    "[]";
to_json(L) when is_list(L) ->
    case io_lib:printable_list(L) of
        true ->
            "\"" ++ L ++ "\"";
        _ ->
            Items = lists:map(fun to_json/1, L),
            "[" ++ string:join(Items, ",") ++ "]"
    end.