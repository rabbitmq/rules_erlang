#!/usr/bin/env escript
%% -*- erlang -*-
%%! -nocookie
-mode(compile).

-export([main/1]).

-ifdef(TEST).
-export([parse/3,
         parse_command/1]).
-endif.

-type macro() :: [atom() | {atom(), term()}].

-spec main([string()]) -> no_return().
main(Args) ->
    case io:get_line("") of
        eof ->
            ok;
        Line ->
            Command = string:trim(Line, trailing, "\n"),
            %% io:format(standard_error, "Command: ~p~n", [Command]),
            #{path := Filename,
              macros := Macros,
              includes := Includes} = parse_command(Command),
            Map = parse(Filename, Macros, Includes),
            %% io:format(standard_error, "Map: ~p~n", [Map]),
            Json = to_json(Map),
            io:format("~s", [Json]),
            % signal to hex_metadata.go that the json is written
            io:format(<<0>>),
            main(Args)
    end.

-spec parse_command(string()) -> #{path := string(), macros := [macro()], includes := [string()]}.
parse_command("{" ++ Tail) ->
    case string:reverse(Tail) of
        "}" ++ Middle ->
            Pairs = string:split(string:reverse(Middle), ",", all),
            maps:from_list([begin
                                [K, V] = string:split(Pair, ":"),
                                case K of
                                    "\"path\"" ->
                                        {path, parse_json_string(V)};
                                    "\"macros\"" ->
                                        {macros, parse_macros(V)};
                                    "\"includes\"" ->
                                        {includes, parse_includes(V)}
                                end
                            end || Pair <- Pairs])
    end.

parse_macros("{}") ->
    [];
parse_macros("{" ++ Tail) ->
    case string:reverse(Tail) of
        "}" ++ Middle ->
            Pairs = string:split(string:reverse(Middle), ",", all),
            [begin
                 [K, V] = string:split(Pair, ":"),
                 Name = parse_json_string(K),
                 case V of
                     "null" ->
                         list_to_atom(Name);
                     _ ->
                         {list_to_atom(Name), parse_json_string(V)}
                 end
             end || Pair <- Pairs]
    end.

parse_includes("[]") ->
    [];
parse_includes("[" ++ Tail) ->
    case string:reverse(Tail) of
        "]" ++ Middle ->
            Elems = string:split(string:reverse(Middle), ",", all),
            [parse_json_string(Elem) || Elem <- Elems]
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
record_attr(E, _, compile, {parse_transform, Dep}) ->
    ets:insert(E, {parse_transform, Dep});
record_attr(E, _, compile, Opts) when is_list(Opts) ->
    case proplists:get_value(parse_transform, Opts) of
        undefined -> ok;
        Dep -> ets:insert(E, {parse_transform, Dep})
    end;
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
record_attr(_, _, _Name, _Value) ->
    %% io:format(standard_error, "record_attr: ~p:~p~n", [Name, Value]),
    ok.

record_expression(E, {remote, _, {atom, _, M}, {atom, _, F}}) ->
    %% io:format(standard_error, "remote: ~p:~p~n", [M, F]),
    ets:insert(E, {call, M, F});
record_expression(E, {call, _, F, Args}) ->
    lists:foreach(
      fun (Arg) -> record_expression(E, Arg) end,
      Args),
    record_expression(E, F);
record_expression(E, {'fun', _, {clauses, Clauses}}) ->
    lists:foreach(
      fun (Clause) -> record_expression(E, Clause) end,
      Clauses);
record_expression(E, {clause, _, Args, _Guards, Expressions}) ->
    lists:foreach(
      fun (Arg) -> record_expression(E, Arg) end,
      Args),
    lists:foreach(
      fun (Expression) -> record_expression(E, Expression) end,
      Expressions);
record_expression(E, {block, _, Expressions}) ->
    lists:foreach(
      fun (Expression) -> record_expression(E, Expression) end,
      Expressions);
record_expression(E, {'case', _, Arg, Expressions}) ->
    %% io:format(standard_error, "case: ~p~n", [Arg]),
    record_expression(E, Arg),
    lists:foreach(
      fun (Expression) -> record_expression(E, Expression) end,
      Expressions);
record_expression(E, {'try', _, Expressions, _, Clauses, Afters}) ->
    lists:foreach(
      fun (Expression) -> record_expression(E, Expression) end,
      Expressions),
    lists:foreach(
      fun (Clause) -> record_expression(E, Clause) end,
      Clauses),
    lists:foreach(
      fun (After) -> record_expression(E, After) end,
      Afters);
record_expression(E, {match, _, Lhs, Rhs}) ->
    record_expression(E, Lhs),
    record_expression(E, Rhs);
record_expression(E, {tuple, _, Elements}) ->
    lists:foreach(
      fun (Element) -> record_expression(E, Element)end,
      Elements);
record_expression(E, {map, _, Assocs}) ->
    lists:foreach(
      fun
          ({map_field_assoc, _, Lhs, Rhs}) ->
              record_expression(E, Lhs),
              record_expression(E, Rhs);
          (_) ->
              ok
      end,
      Assocs);
record_expression(E, {cons, _, Head, Tail}) ->
    record_expression(E, Head),
    record_expression(E, Tail);
record_expression(E, {op, _, _, Lhs, Rhs}) ->
    record_expression(E, Lhs),
    record_expression(E, Rhs);
record_expression(_, _Exp) ->
    %% io:format(standard_error, "E: ~p~n", [Exp]),
    ok.

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
note_form(E, _, {function, _, _, _, Clauses} = _AbsData) ->
    %% io:format(standard_error, "AbsData: ~p~n", [AbsData]),
    lists:foreach(
      fun (Clause) -> record_expression(E, Clause) end,
      Clauses);
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
          ({parse_transform, Dep}, #{parse_transform := Transforms} = Acc) ->
              Acc#{parse_transform := [Dep | Transforms]};
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
        parse_transform => [],
        call => #{}
       },
      E).

-spec parse(file:name(), [macro()], [string()]) -> map() | 'null'.
parse(File, Macros, Includes) ->
    Opts0 = [],
    Opts1 = case Macros of
                Ms when length(Ms) > 0 ->
                    [{macros, Ms} | Opts0];
                _ ->
                    Opts0
            end,
    Opts = case Includes of
               Is when length(Is) > 0 ->
                   [{includes, Is} | Opts1];
               _ ->
                   Opts1
           end,
    case epp:parse_file(File, Opts) of
        {ok, Forms} ->
            %% io:format(standard_error, "Forms: ~p~n", [Forms]),
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
