-module(coverdata_to_lcov).

-mode(compile).

-export([main/1]).

-spec main([string()]) -> no_return().

main([TestName, CoverdataFile, LcovFile]) ->
    ScriptName = filename:basename(escript:script_name()),
    io:format(standard_error, "~s: converting ~s to lcov...~n",
              [ScriptName, CoverdataFile]),
    {ok, Cwd} = file:get_cwd(),

    ok = cover:import(CoverdataFile),
    Modules = cover:imported_modules(),
    
    {ok, S} = file:open(LcovFile, [write]),
    io:format(S, "TN:~s~n", [TestName]),
    lists:foreach(
      fun (Module) ->
              Path = code:which(Module),
              io:format(S, "SF:~s~n", [Path]),

              {ok, Lines} = cover:analyse(Module, calls, line),
              {LH, LF} = lists:foldl(
                           fun ({{_M, N}, Calls}, {LinesHit, LinesFound}) ->
                                   case Calls of
                                       0 ->
                                           ok;
                                       _ ->
                                           RelPath = relative_path(Path, Cwd),
                                           io:format(standard_error,
                                                     "~s: ~s:~B calls ~B~n",
                                                     [ScriptName, RelPath, N, Calls])
                                   end,
                                   io:format(S, "DA:~B,~B~n", [N, Calls]),
                                   {LinesHit + is_hit(Calls), LinesFound + 1}
                           end, {0, 0}, Lines),
              io:format(S, "LH:~B~n", [LH]),
              io:format(S, "LF:~B~n", [LF]),
              io:format(S, "end_of_record~n", [])
      end, Modules),
    file:close(S).

relative_path(Path, Cwd) ->
    case string:prefix(Path, Cwd ++ "/") of
        nomatch -> Path;
        RelPath -> RelPath
    end.

is_hit(0) ->
    0;
is_hit(_) ->
    1.
