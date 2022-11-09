#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname erl_attrs_to_json -nocookie
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

record_attr(E, _F, _Dir, behavior, Dep) ->
    ets:insert(E, {behaviour, Dep});
record_attr(E, _F, _Dir, behaviour, Dep) ->
    ets:insert(E, {behaviour, Dep});
%% record_attr(E, _F, _Dir, compile, {parse_transform, Dep}) ->
%%     ets:insert(E, {Dep});
%% record_attr(E, _F, _Dir, compile, Opts) when is_list(Opts) ->
%%     case proplists:get_value(parse_transform, Opts) of
%%         undefined -> ok;
%%         Dep -> ets:insert(E, {Dep})
%%     end;
record_attr(E, _F, _Dir, include, Path) ->
    ets:insert(E, {include, Path});
record_attr(E, _F, _Dir, include_lib, Path) ->
    ets:insert(E, {include_lib, Path});
record_attr(_, _, _, _, _) ->
    ok.

walk_forms(E, F, Dir, Fd, StartLocation) ->
    case io:parse_erl_form(Fd, undefined, StartLocation) of
        {ok, AbsData, EndLocation} ->
            case AbsData of
                {attribute, _, Key, Value} ->
                    % io:format(standard_error, "AbsData: ~p~n", [AbsData]),
                    record_attr(E, F, Dir, Key, Value),
                    walk_forms(E, F, Dir, Fd, EndLocation);
                _ ->
                    walk_forms(E, F, Dir, Fd, EndLocation)
            end;
        {eof, _} ->
            file:close(Fd);
        {error, _} ->
            file:close(Fd);
        {error, _, ErrorLocation} ->
            walk_forms(E, F, Dir, Fd, ErrorLocation)
    end.

deps(E, _F) ->
    ets:foldl(
      fun
          ({include_lib, Path}, #{include_lib := IncludeLib} = Acc) ->
              Acc#{include_lib := [Path | IncludeLib]};
          ({include, Path}, #{include := Include} = Acc) ->
              Acc#{include := [Path | Include]};
          ({behaviour, Dep}, #{behaviour := Behaviours} = Acc) ->
              Acc#{behaviour := [Dep | Behaviours]};
          (_, Acc) ->
              Acc
      end,
      #{
        include_lib => [],
        include => [],
        behaviour => []
       },
      E).

parse(File) ->
    E = ets:new(makedep, [bag]),
    F = ets:new(visitedfiles, [bag]),

    {ok, Fd} = file:open(File, [read]),

    walk_forms(E, F, filename:dirname(File), Fd, 0),

    Map = deps(E, F),
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
