-module(coverdata_to_lcov).

-mode(compile).

-export([main/1]).

-spec main([string()]) -> no_return().

main([CoverdataFile, LcovFile]) ->
    ScriptName = filename:basename(escript:script_name()),
    io:format(standard_error, "~s: converting ~s to lcov...~n",
              [ScriptName, CoverdataFile]),

    {ok, Cwd} = file:get_cwd(),
    {ok, ExecRoot} = find_execroot(Cwd),

    ok = cover:import(CoverdataFile),
    Modules = cover:imported_modules(),
    
    {ok, S} = file:open(LcovFile, [write]),
    lists:foreach(
      fun (Module) ->
              case guess_source_file(Module, ExecRoot) of
                  {ok, SF} ->
                      io:format(S, "SF:~s~n", [SF]),

                      {ok, Lines} = cover:analyse(Module, calls, line),
                      {LH, LF} = lists:foldl(
                                   fun ({{_M, N}, Calls}, {LinesHit, LinesFound}) ->
                                           case Calls of
                                               0 ->
                                                   ok;
                                               _ ->
                                                   io:format(standard_error,
                                                             "~s: ~s:~B calls ~B~n",
                                                             [ScriptName, SF, N, Calls])
                                           end,
                                           io:format(S, "DA:~B,~B~n", [N, Calls]),
                                           {LinesHit + is_hit(Calls), LinesFound + 1}
                                   end, {0, 0}, Lines),
                      io:format(S, "LH:~B~n", [LH]),
                      io:format(S, "LF:~B~n", [LF]),
                      io:format(S, "end_of_record~n", []);
                  _ ->
                      io:format(standard_error,
                                "~s: WARNING: failed to locate source file for module ~p~n",
                                [ScriptName, Module])
              end
      end, Modules),
    file:close(S).

find_execroot(Dir) ->
    find_execroot(Dir, filename:dirname(Dir)).

find_execroot("/", "/") ->
    not_found;
find_execroot(Dir, Parent) ->
    case {filename:basename(Dir), filename:basename(Parent)} of
        {"_main", "execroot"} ->
            {ok, Dir};
        _ ->
            find_execroot(filename:dirname(Dir), filename:dirname(Parent))
    end.

guess_source_file(Module, SourceRoot) ->
    case source_file(Module, SourceRoot, "apps") of
        not_found ->
            source_file(Module, SourceRoot, "deps");
        P ->
            P
    end.

source_file(Module, SourceRoot, AppsDir) ->
    Pattern = filename:join([AppsDir, "*", "src", atom_to_list(Module) ++ ".erl"]),
    case filelib:wildcard(Pattern, SourceRoot) of
        [Path] -> {ok, Path};
        _ -> generated_source_file(Module, SourceRoot, AppsDir)
    end.

generated_source_file(Module, SourceRoot, AppsDir) ->
    Pattern = filename:join(["bazel-out", "*", "bin", AppsDir, "*", "src", atom_to_list(Module) ++ ".erl"]),
    case filelib:wildcard(Pattern, SourceRoot) of
        [Path] ->
            AppName = filename:basename(filename:dirname(filename:dirname(Path))),
            {ok, filename:join(["bazel-bin", AppsDir, AppName, "src", filename:basename(Path)])};
        _ ->
            not_found
    end.

is_hit(0) ->
    0;
is_hit(_) ->
    1.
