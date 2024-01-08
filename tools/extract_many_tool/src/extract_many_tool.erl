-module(extract_many_tool).

-export([
    main/1
   ]).

-type app_data() :: #{src_path := string(),
                      priv := [string()],
                      outs := [string()]}.

-spec main([string()]) -> no_return().
main(["--apps_json", AppsJsonFile, "--out", OutputTar | AppsStrings]) ->
    case os:getenv("ERLANG_HOME") of
        undefined ->
            io:format(standard_error,
                      "ERLANG_HOME must be set~n",
                      []),
            exit(1);
        _ ->
            ok
    end,

    AppsData = apps_data(AppsJsonFile),
    %% io:format("AppsData: ~p~n", [AppsData]),

    io:format("Creating ~p~n", [OutputTar]),

    Apps = lists:map(fun list_to_atom/1, AppsStrings),

    io:format("  Copying Apps and their dependencies: ~p~n", [Apps]),

    AppGraph = digraph:new([acyclic]),

    add_deps(AppGraph, lists:usort(Apps)),
    
    AllApps = digraph_utils:preorder(AppGraph),
    
    io:format("  Complete Apps list: ~p~n", [AllApps]),

    %% T0 = os:cmd("/usr/local/bin/tree -L 3 " ++ StagingDir),
    %% io:format(standard_error,
    %%           "~s~n", [T0]),

    Entries = lists:flatmap(
                fun (App) ->
                        app_entries(App, maps:get(App, AppsData))
                end, AllApps),

    %% T1 = os:cmd("/usr/local/bin/tree -L 4 " ++ StagingDir),
    %% io:format(standard_error,
    %%           "~s~n", [T1]),

    ok = erl_tar:create(OutputTar, Entries, [dereference]),
    io:format("Archive created ~p~n", [OutputTar]);
main([]) ->
    exit(1).

add_deps(_, []) ->
    ok;
add_deps(G, [App | Rest]) ->
    case is_otp_application(App) of
        true ->
            io:format("    Skipping ~p as it's an otp application~n",
                      [App]),
            add_deps(G, Rest);
        false ->
            digraph:add_vertex(G, App),
            io:format("    Checking deps for ~p~n", [App]),
            Deps = app_deps(App),
            lists:foreach(
              fun(D) ->
                      digraph:add_vertex(G, D)
              end,
              Deps),
            lists:foreach(
              fun(Dep) ->
                      digraph:add_edge(G, App, Dep)
              end, Deps),
            add_deps(G, lists:usort(Rest ++ Deps))
    end.

is_otp_application(App) ->
    DotAppFile = dot_app_file(App),
    case string:prefix(DotAppFile, os:getenv("ERLANG_HOME")) of
        nomatch ->
            false;
        _ ->
            true
    end.

app_deps(App) ->
    DotAppFile = dot_app_file(App),
    {ok, [{application, _, Props}]} = file:consult(DotAppFile),
    {_, Apps} = lists:keyfind(applications, 1, Props),
    lists:filter(
      fun (D) -> false == is_otp_application(D) end,
      Apps).

dot_app_file(App) ->
    AppFile = atom_to_list(App) ++ ".app",
    case code:where_is_file(AppFile) of
        non_existing ->
            error({non_existing, AppFile});
        DotAppFile ->
            DotAppFile
    end.

-spec app_entries(atom(), app_data()) -> [{string(), filename:name()}].
app_entries(App, #{src_path := SrcPath, priv := Priv}) ->
    DotAppFile = dot_app_file(App),
    %% {ok, [{application, _, Props}]} = file:consult(DotAppFile),
    %% {_, Apps} = lists:keyfind(applications, 1, Props),
    SrcDir = filename:dirname(filename:dirname(DotAppFile)),
    SrcFiles = lists:filter(
                 fun (Src) ->
                         filename:extension(Src) /= ".erl"
                 end, list_recursive(SrcDir)),
    BeamEntries = lists:map(
                    fun(SrcFile) ->
                            RelPath = string:prefix(SrcFile, SrcDir ++ "/"),
                            true = (RelPath /= nomatch),
                            DestFile = filename:join(atom_to_list(App), RelPath),
                            {DestFile, SrcFile}
                    end, SrcFiles),
    PrivEntries = lists:map(
                    fun (PrivFile) ->
                            RelPath = string:prefix(PrivFile, SrcPath ++ "/"),
                            true = (RelPath /= nomatch),
                            DestFile = filename:join(atom_to_list(App), RelPath),
                            {DestFile, PrivFile}
                    end, Priv),
    BeamEntries ++ PrivEntries.

list_recursive(Src) ->
    case filelib:is_dir(Src) of
        true ->
            {ok, Files} = file:list_dir(Src),
            lists:flatmap(
              fun (File) ->
                      list_recursive(filename:join(Src, File))
              end, Files);
        false ->
            [Src]
    end.

-spec apps_data(filename:name()) -> #{atom() := app_data()}.
apps_data(JsonFile) ->
    {ok, Json} = file:read_file(JsonFile),
    {ok, Decoded} = thoas:decode(Json),
    maps:fold(
     fun (AppBin, #{<<"src_path">> := SrcPath,
                    <<"priv">> := Priv,
                    <<"outs">> := Outs}, Acc) ->
             SP = case SrcPath of null -> null; _ -> binary_to_list(SrcPath) end,
             Acc#{binary_to_atom(AppBin) => #{src_path => SP,
                                              priv => lists:map(fun binary_to_list/1, Priv),
                                              outs => lists:map(fun binary_to_list/1, Outs)}}
     end, #{}, Decoded).
