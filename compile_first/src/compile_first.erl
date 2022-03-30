-module(compile_first).

-mode(compile).

-export([main/1]).

-spec main([string()]) -> no_return().
main(ErlFiles) ->
    %% This can be a simplified analysis, as deps
    %% are already compiled. We just need to cover
    %% things like behaviors within the set of
    %% ErlFiles

    E = ets:new(makedep, [bag]),

    [begin
         case file:open(F, [read]) of
             {ok, Fd} ->
                 walk_forms(E, Fd, 0)
         end
     end || F <- ErlFiles],

    %% any item in the table that is also one of the
    %% ErlFiles should be compiled first
    CompileFirst = lists:filter(fun (F) ->
                                        Mod = list_to_atom(filename:basename(F, ".erl")),
                                        ets:member(E, Mod)
                                end, ErlFiles),

    io:format("~s", [string:join(CompileFirst, " ")]),

    halt().

walk_forms(E, Fd, StartLocation) ->
    case io:parse_erl_form(Fd, undefined, StartLocation) of
        {ok, AbsData, EndLocation} ->
            case AbsData of
                {attribute, _, Key, Value} ->
                    record_attr(E, Key, Value),
                    walk_forms(E, Fd, EndLocation);
                _ ->
                    walk_forms(E, Fd, EndLocation)
            end;
        {eof, _} ->
            file:close(Fd);
        {error, _} ->
            file:close(Fd);
        {error, _, ErrorLocation} ->
            walk_forms(E, Fd, ErrorLocation)
    end.

record_attr(E, behavior, Dep) ->
    ets:insert(E, {Dep});
record_attr(E, behaviour, Dep) ->
    ets:insert(E, {Dep});
record_attr(E, compile, {parse_transform, Dep}) ->
    ets:insert(E, {Dep});
record_attr(E, compile, Opts) when is_list(Opts) ->
    case proplists:get_value(parse_transform, Opts) of
        undefined -> ok;
        Dep -> ets:insert(E, {Dep})
    end;
record_attr(_, _, _) ->
    ok.
