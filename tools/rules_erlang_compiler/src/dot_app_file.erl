-module(dot_app_file).

-include("types.hrl").

-export([render/3]).

-spec render(atom(), target(), file:name()) -> ok.
render(AppName,
       #{app_src := AppSrc, outs := Outs} = Target,
       DestDir) ->
    Contents = case AppSrc of
                   null ->
                       {application, AppName, []};
                   _ ->
                       {ok, [AppInfo]} = file:consult(AppSrc),
                       AppInfo
               end,
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
    {application, AppName, Props0} = Contents,
    Props1 = lists:keystore(modules, 1, Props0, {modules, Modules}),
    Props = case lists:keymember(applications, 1, Props1) of
                true ->
                    Props1;
                false ->
                    Deps = sets:to_list(maps:get(deps, Target, sets:new())),
                    Apps = [kernel, stdlib] ++ lists:map(fun list_to_atom/1, Deps),
                    io:format("Target: ~p~n", [Target]),
                    lists:keystore(applications, 1, Props1,
                                   {applications, Apps})
            end,
    Dest = filename:join([DestDir, AppName, "ebin", atom_to_list(AppName) ++ ".app"]),
    ok = file:write_file(Dest,
                         io_lib:format("~tp.~n", [{application,
                                                   AppName,
                                                   Props}])).
