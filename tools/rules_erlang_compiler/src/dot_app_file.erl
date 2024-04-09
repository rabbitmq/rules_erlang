-module(dot_app_file).

-include("types.hrl").

-export([render/3]).

-spec render(atom(), target(), file:name()) -> {ok, warnings_list()}.
render(
    AppName,
    #{app_src := AppSrc, outs := Outs} = Target,
    DestDir
) ->
    Deps = maps:get(deps, Target, sets:new()),
    {ok, [Contents]} = file:consult(AppSrc),
    {application, AppName, Props0} = Contents,

    {applications, Applications} = lists:keyfind(applications, 1, Props0),
    AllApplications =
        case lists:keyfind(optional_applications, 1, Props0) of
            {optional_applications, Optional} ->
                Applications ++ Optional;
            false ->
                Applications
        end,
    Warnings = sets:fold(
        fun(Dep, Acc) ->
            case lists:member(Dep, AllApplications) of
                true ->
                    Acc;
                false ->
                    [
                        io_lib:format("~p may depend on ~p but the application is not listed", [
                            AppName, Dep
                        ])
                        | Acc
                    ]
            end
        end,
        [],
        Deps
    ),

    Modules = lists:filtermap(
        fun(Out) ->
            case filename:extension(Out) of
                ".beam" ->
                    {true, list_to_atom(filename:basename(Out, ".beam"))};
                _ ->
                    false
            end
        end,
        Outs
    ),
    Props1 = lists:keystore(modules, 1, Props0, {modules, Modules}),
    Props = ensure_relx_compliant(Props1),
    Dest = filename:join([DestDir, AppName, "ebin", atom_to_list(AppName) ++ ".app"]),
    ok = file:write_file(
        Dest,
        io_lib:format("~tp.~n", [{application, AppName, Props}])
    ),
    case Warnings of
        [] ->
            {ok, []};
        _ ->
            {ok, [{Dest, Warnings}]}
    end.


ensure_relx_compliant(AppData) ->
    ensure_registered(ensure_string_vsn(AppData)).

%% https://github.com/erlware/relx/issues/32
ensure_registered(AppData) ->
    case lists:keyfind(registered, 1, AppData) of
        false ->
            [{registered, []} | AppData];
        {registered, _} ->
            AppData
    end.

%% https://github.com/for-GET/jesse/blob/master/src/jesse.app.src#L4
ensure_string_vsn(AppData) ->
    case proplists:get_value(vsn, AppData, undefined) of
        undefined ->
            AppData;
        VSN when is_atom(VSN)->
            proplists:delete(vsn, AppData) ++ [{vsn, atom_to_list(VSN)}];
        _ ->
            AppData
    end.
