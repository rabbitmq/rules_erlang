#!/usr/bin/env escript
%% -*- erlang -*-
%%! -nocookie
-mode(compile).

-export([main/1]).

-ifdef(TEST).
-export([parse/2]).
-endif.

-spec main([string()]) -> no_return().
main(Args) ->
    case io:get_line("") of
        eof ->
            ok;
        Line ->
            #{path := Filename, test := Test} = parse_command(string:trim(Line, trailing, "\n")),
            Map = parse(Filename, Test),
            %% io:format(standard_error, "Map: ~p~n", [Map]),
            Json = to_json(Map),
            io:format("~s", [Json]),
            % signal to hex_metadata.go that the json is written
            io:format(<<0>>),
            main(Args)
    end.

-spec parse_command(string()) -> map().
parse_command("{" ++ Tail) ->
    case string:reverse(Tail) of
        "}" ++ Middle ->
            Pairs = string:split(string:reverse(Middle), ",", all),
            maps:from_list([begin
                [K, V] = string:split(Pair, ":"),
                case K of
                    "\"path\"" ->
                        {path, parse_json_string(V)};
                    "\"test\"" ->
                        {test, list_to_atom(V)}
                end
            end || Pair <- Pairs])
    end.

parse_json_string("\"" ++ Tail) ->
    case string:reverse(Tail) of
        "\"" ++ Middle ->
            string:reverse(Middle)
    end.

drop_common([C | L], [C | R]) ->
    drop_common(L, R);
drop_common(L, R) ->
    {L, R}.

relpath(Base, File) ->
    BaseComponents = filename:split(Base),
    FileComponents = filename:split(File),
    {BC, FC} = drop_common(BaseComponents, FileComponents),
    case {BC, FC} of
        {[_], [R]} -> R;
        {[_], C} -> filename:join(C);
        {BC, FC} -> filename:join([".." || _ <- BC] ++ FC)
    end.

record_attr(E, _, behavior, Dep) ->
    ets:insert(E, {behaviour, Dep});
record_attr(E, _, behaviour, Dep) ->
    ets:insert(E, {behaviour, Dep});
%% record_attr(E, compile, {parse_transform, Dep}) ->
%%     ets:insert(E, {Dep});
%% record_attr(E, compile, Opts) when is_list(Opts) ->
%%     case proplists:get_value(parse_transform, Opts) of
%%         undefined -> ok;
%%         Dep -> ets:insert(E, {Dep})
%%     end;
record_attr(E, File, file, {Path, _}) when File =/= Path ->
    case string:reverse(Path) of
        "lrh." ++ _ ->
            RelPath = relpath(File, Path),
            ets:insert(E, {include, RelPath});
        _ ->
            ok
    end;
record_attr(E, _, include, Path) ->
    ets:insert(E, {include, Path});
record_attr(E, _, include_lib, Path) ->
    ets:insert(E, {include_lib, Path});
record_attr(_, _, _, _) ->
    ok.

record_expression(E, {call, _, {remote, _, {atom, _, M}, {atom, _, F}}, _Args}) ->
    %% TODO: handle anonymous functions in _Args
    ets:insert(E, {call, M, F});
record_expression(_, {call, _, _, _} = _Call) ->
    %% io:format(standard_error, "Ignoring Call: ~p~n", [Call]),
    ok;
record_expression(_, _) ->
    ok.

record_clause(_, {clause, _, [], _, []}) ->
    ok;
record_clause(E, {clause, N, [Arg | Rest], O, Expressions}) ->
    case Arg of
        {'fun' = Name, _, Clauses} ->
            record_function(E, Name, Clauses);
        _ ->
            record_clause(E, {clause, N, Rest, O, Expressions})
    end;
record_clause(E, {clause, N, [], O, [Expression | Rest]}) ->
    record_expression(E, Expression),
    record_clause(E, {clause, N, [], O, Rest}).

record_function(_, _, []) ->
    ok;
record_function(E, Name, [Clause | Rest]) ->
    record_clause(E, Clause),
    record_function(E, Name, Rest).

note_form(_, _, {eof, _}) ->
    ok;
note_form(E, File, {error, {_, epp, {include, lib, Dep}}}) ->
    record_attr(E, File, include_lib, Dep);
note_form(E, File, {error, {_, epp, {include, file, Dep}}}) ->
    record_attr(E, File, include, Dep);
note_form(_, _, {error, _Reason}) ->
    %% io:format(standard_error, "error: ~p~n", [Reason]),
    ok;
note_form(E, File, {attribute, _, Key, Value} = _AbsData) ->
    %% io:format(standard_error, "AbsData: ~p~n", [AbsData]),
    record_attr(E, File, Key, Value);
note_form(E, _, {function, _, Name, _, Clauses} = _AbsData) ->
    %% io:format(standard_error, "AbsData: ~p~n", [AbsData]),
    record_function(E, Name, Clauses);
note_form(_, _, _AbsData) ->
    %% io:format(standard_error, "AbsData: ~p~n", [AbsData]),
    ok.

deps(E) ->
    ets:foldl(
      fun
          ({include_lib, Path}, #{include_lib := IncludeLib} = Acc) ->
              Acc#{include_lib := [Path | IncludeLib]};
          ({include, Path}, #{include := Include} = Acc) ->
              Acc#{include := [Path | Include]};
          ({behaviour, Dep}, #{behaviour := Behaviours} = Acc) ->
              Acc#{behaviour := [Dep | Behaviours]};
          ({call, M, F}, #{call := Calls0} = Acc) ->
              Calls = maps:update_with(M,
                                       fun(Fs) ->
                                               case lists:member(F, Fs) of
                                                   false ->
                                                       [F | Fs];
                                                   _ ->
                                                       Fs
                                               end
                                       end,
                                       [F],
                                       Calls0),
              Acc#{call := Calls};
          (_, Acc) ->
              Acc
      end,
      #{
        include_lib => [],
        include => [],
        behaviour => [],
        call => #{}
       },
      E).

-spec parse(filename:any(), boolean()) -> map() | 'null'.
parse(File, Test) ->
    Opts = case Test of
        true ->
            [{macros, ['TEST']}];
        false ->
            []
    end,
    case epp:parse_file(File, Opts) of
        {ok, Forms} ->
            E = ets:new(makedep, [bag]),
            lists:foreach(fun (Form) -> note_form(E, File, Form) end, Forms),
            deps(E);
        {error, Reason} ->
            io:format(standard_error, "~s: error opening ~s: ~p~n",
                      [filename:basename(escript:script_name()), File, Reason]),
            null
    end.

-spec to_json(term()) -> string().
to_json(null) ->
    "null";
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
