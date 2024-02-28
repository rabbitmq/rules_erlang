-module(extract_many_tool).

-export([
    main/1
   ]).

-type app_data() :: #{priv := [string()],
                      outs := [string()]}.

-type apps_data() :: #{atom() := app_data()}.

-spec main([string()]) -> no_return().
main(["--apps_json", AppsJsonFile,
      "--out", OutputTar,
      "--versioned_dirs", VersionedDirsString | AppsStrings]) ->
    case os:getenv("ERLANG_HOME") of
        false ->
            io:format(standard_error,
                      "ERLANG_HOME must be set~n",
                      []),
            exit(1);
        _ ->
            ok
    end,

    VersionedDirs = parse_bool(VersionedDirsString),

    AppsData = apps_data(AppsJsonFile),
    %% io:format("AppsData: ~p~n", [AppsData]),

    io:format("Creating ~p~n", [OutputTar]),

    Apps = lists:map(fun list_to_atom/1, AppsStrings),

    io:format("  Copying Apps and their dependencies: ~p~n", [Apps]),

    AppGraph = digraph:new([acyclic]),

    add_deps(AppGraph, AppsData, lists:usort(Apps), sets:new()),
    
    AllApps = lists:sort(digraph:vertices(AppGraph)),
    
    io:format("  Complete Apps list: ~p~n", [AllApps]),

    %% T0 = os:cmd("/usr/local/bin/tree -L 3 " ++ StagingDir),
    %% io:format(standard_error,
    %%           "~s~n", [T0]),

    Entries = lists:flatmap(
                fun (App) ->
                        %% io:format("~p~n", [App]),
                        app_entries(App, AppsData, VersionedDirs)
                end, AllApps),

    %% T1 = os:cmd("/usr/local/bin/tree -L 4 " ++ StagingDir),
    %% io:format(standard_error,
    %%           "~s~n", [T1]),

    ok = erl_tar:create(OutputTar, Entries, [dereference]),
    io:format("Archive created ~p~n", [OutputTar]);
main([]) ->
    exit(1).

-spec add_deps(digraph:graph(), apps_data(), [atom()], sets:set()) -> ok.
add_deps(_, _, [], _) ->
    ok;
add_deps(G, AppsData, [App | Rest], Visited) ->
    case is_otp_application(App, AppsData) of
        true ->
            io:format("    Skipping ~p as it's an otp application~n",
                      [App]),
            add_deps(G, AppsData, Rest, Visited);
        false ->
            case sets:is_element(App, Visited) of
                false ->
                    digraph:add_vertex(G, App),
                    io:format("    Checking deps for ~p~n", [App]),
                    Deps = app_deps(App, AppsData),
                    lists:foreach(
                      fun(D) ->
                              digraph:add_vertex(G, D)
                      end,
                      Deps),
                    lists:foreach(
                      fun(Dep) ->
                              digraph:add_edge(G, App, Dep)
                      end, Deps),
                    add_deps(G,
                             AppsData,
                             lists:usort(Rest ++ Deps),
                             sets:add_element(App, Visited));
                true ->
                    io:format("    ~p already checked~n", [App]),
                    add_deps(G, AppsData, Rest, Visited)
            end
    end.

-spec is_otp_application(atom(), apps_data()) -> boolean().
is_otp_application(App, AppsData) ->
    DotAppFile = dot_app_file(App, AppsData),
    case string:prefix(DotAppFile, os:getenv("ERLANG_HOME")) of
        nomatch ->
            false;
        _ ->
            true
    end.

-spec app_deps(atom(), apps_data()) -> [atom()].
app_deps(App, AppsData) ->
    DotAppFile = dot_app_file(App, AppsData),
    {ok, [{application, _, Props}]} = file:consult(DotAppFile),
    {_, Apps} = lists:keyfind(applications, 1, Props),
    lists:filter(
      fun (D) -> false == is_otp_application(D, AppsData) end,
      Apps).

-spec dot_app_file(atom(), apps_data()) -> file:filename().
dot_app_file(App, AppsData) ->
    case AppsData of
        #{App := #{outs := Outs}} ->
            case lists:search(fun (F) -> filename:extension(F) == ".app" end, Outs) of
                {value, DotAppFile} ->
                    DotAppFile;
                false ->
                    error({no_dot_app_file, App})
            end;
        _ ->
            AppFile = atom_to_list(App) ++ ".app",
            case code:where_is_file(AppFile) of
                non_existing ->
                    error({non_existing, AppFile});
                DotAppFile ->
                    DotAppFile
            end
    end.

-spec app_entries(atom(), apps_data(), boolean()) -> [{string(), file:filename()}].
app_entries(App, AppsData, VersionedDirs) ->
    #{App := #{priv := Priv}} = AppsData,
    DotAppFile = dot_app_file(App, AppsData),
    AppDirname = case VersionedDirs of
                     true ->
                         {ok, [{application, _, Props}]} = file:consult(DotAppFile),
                         {vsn, Vsn} = lists:keyfind(vsn, 1, Props),
                         atom_to_list(App) ++ "-" ++ Vsn;
                     false ->
                         atom_to_list(App)
                 end,
    SrcDir = filename:dirname(filename:dirname(DotAppFile)),
    SrcFiles = lists:filter(
                 fun (Src) ->
                         filename:extension(Src) /= ".erl"
                 end, list_recursive(SrcDir)),
    BeamEntries = lists:map(
                    fun(SrcFile) ->
                            RelPath = string:prefix(SrcFile, SrcDir ++ "/"),
                            true = (RelPath /= nomatch),
                            DestFile = filename:join(AppDirname, RelPath),
                            {DestFile, SrcFile}
                    end, SrcFiles),
    PrivEntries = lists:map(
                    fun (PrivFile) ->
                            %% This heuristic for the relative path might not be sufficient
                            [_, RelPath] = string:split(PrivFile, atom_to_list(App) ++ "/"),
                            DestFile = filename:join(AppDirname, RelPath),
                            {DestFile, PrivFile}
                    end, Priv),
    BeamEntries ++ PrivEntries.

-spec list_recursive(file:filename()) -> [file:filename()].
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

-spec apps_data(file:filename()) -> apps_data().
apps_data(JsonFile) ->
    {ok, Json} = file:read_file(JsonFile),
    {ok, Decoded} = thoas:decode(Json),
    maps:fold(
     fun (AppBin, #{<<"priv">> := Priv,
                    <<"outs">> := Outs}, Acc) ->
             Acc#{binary_to_atom(AppBin) => #{priv => lists:map(fun binary_to_list/1, Priv),
                                              outs => lists:map(fun binary_to_list/1, Outs)}}
     end, #{}, Decoded).

parse_bool("true") ->
    true;
parse_bool("false") ->
    false.
