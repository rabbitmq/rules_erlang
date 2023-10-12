-module(coverdata_to_lcov).

-mode(compile).

-export([main/1]).

-spec main([string()]) -> no_return().

main([TestName, CoverdataFile, LcovFile]) ->
    io:format(standard_error, "~s: converting ~s to lcov...~n",
              [filename:basename(escript:script_name()), CoverdataFile]),
    ok = cover:import(CoverdataFile),
    Modules = cover:imported_modules(),
    {result, Ok, _Fail} = cover:analyse(Modules, calls, line),
    
    {ok, S} = file:open(LcovFile, [write]),
    io:format(S, "TN:~s~n", [TestName]),
    %% lists:foreach(
    %%   fun (Module) ->
    %%           {ok, Lines} = cover:analyse(Module, calls, line),
    %%           lists:foreach(
    %%             fun ({{M, N}, Calls}) ->
    %%                     io:format(standard_error, "\t~p~n", [{M, N, Calls}])
    %%             end, Lines)
    %%   end, Modules),
    lists:foreach(
      fun
          ({{M, N} = _Line, Calls}) ->
              io:format(standard_error, "\t~p~n", [{M, N, Calls}])
      end, Ok),
    file:close(S),
    halt(1).
