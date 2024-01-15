-module(dot_app_file).

-include("types.hrl").

-export([render/3]).

-spec render(atom(), target(), file:name()) -> ok.
render(AppName,
       #{app_src := AppSrc, outs := Outs},
       DestDir) ->
    {ok, [Contents]} = file:consult(AppSrc),
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
    Props = lists:keystore(modules, 1, Props0, {modules, Modules}),
    Dest = filename:join([DestDir, AppName, "ebin", atom_to_list(AppName) ++ ".app"]),
    ok = file:write_file(Dest,
                         io_lib:format("~tp.~n", [{application,
                                                   AppName,
                                                   Props}])).
