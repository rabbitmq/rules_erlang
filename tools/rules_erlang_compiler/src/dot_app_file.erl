-module(dot_app_file).

-include("types.hrl").

-export([render/3]).

-spec render(atom(), target(), file:name()) -> {ok, warnings_list()}.
render(AppName,
       #{app_src := AppSrc, outs := Outs} = Target,
       DestDir) ->
    Deps = maps:get(deps, Target, sets:new()),
    {ok, [Contents]} = file:consult(AppSrc),
    {application, AppName, Props0} = Contents,

    {applications, Applications} = lists:keyfind(applications, 1, Props0),
    AllApplications = case lists:keyfind(optional_applications, 1, Props0) of
                          {optional_applications, Optional} ->
                              Applications ++ Optional;
                          false ->
                              Applications
                      end,
    Warnings = sets:fold(
                 fun (Dep, Acc) ->
                         case lists:member(Dep, AllApplications) of
                             true ->
                                 Acc;
                             false ->
                                 [io_lib:format("~p may depend on ~p but the application is not listed", [AppName, Dep]) | Acc]
                         end
                 end, [], Deps),

    Modules = lists:filtermap(
                fun (Out) ->
                        case filename:extension(Out) of
                            ".beam" ->
                                {true,
                                 list_to_atom(filename:basename(Out, ".beam"))};
                            _ ->
                                false
                        end
                end, Outs),
    Props = lists:keystore(modules, 1, Props0, {modules, Modules}),
    Dest = filename:join([DestDir, AppName, "ebin", atom_to_list(AppName) ++ ".app"]),
    ok = file:write_file(Dest,
                         io_lib:format("~tp.~n", [{application,
                                                   AppName,
                                                   Props}])),
    case Warnings of
        [] ->
            {ok, []};
        _ ->
            {ok, [{Dest, Warnings}]}
    end.
