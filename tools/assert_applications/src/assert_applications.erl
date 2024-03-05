-module(assert_applications).

-mode(compile).

-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-spec main([string()]) -> no_return().
main([AppFile | Expected0]) ->
    Expected1 = lists:map(fun list_to_atom/1, Expected0),
    Expected = lists:sort(Expected1),
    io:format("Expecting ~p applications in ~s~n", [Expected, AppFile]),
    {ok, [AppInfo]} = file:consult(AppFile),
    {application, _, Props} = AppInfo,
    {applications, Applications} = lists:keyfind(applications, 1, Props),
    ?assertEqual(Expected, lists:sort(Applications)),
    halt();
main(_) ->
    halt(1).
