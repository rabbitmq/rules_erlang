-module(extract_many_tool).

-export([
    main/1
   ]).

-spec main([string()]) -> no_return().
main([_OutputTar | AppsStrings]) ->
    case os:getenv("ERLANG_HOME") of
        undefined ->
            io:format(standard_error,
                      "ERLANG_HOME must be set~n",
                      []),
            exit(1);
        _EH ->
            %% io:format("ERLANG_HOME -> ~p~n", [EH]),
            ok
    end,

    %% 1. create a tempdir
    %% 2. for each app in Apps, get it's deps
    %% 3. flatten the deps and uniquify
    %% 4. copy each of those deps into the tempdir
    %% 5. tar up tempdir into OutputTar

    Apps = lists:map(fun list_to_atom/1, AppsStrings),

    io:format("Apps: ~p~n", [Apps]),

    _TmpDir = os:getenv("TMPDIR"),

    AppGraph = digraph:new([acyclic]),

    add_deps(AppGraph, lists:usort(Apps)),
    
    AllApps = digraph_utils:preorder(AppGraph),
    
    io:format("AllApps: ~p~n", [AllApps]),


    exit(1);
main([]) ->
    exit(1).

add_deps(_, []) ->
    ok;
add_deps(G, [App | Rest]) ->
    case is_otp_application(App) of
        true ->
            io:format("  Skipping ~p as it's an otp application~n",
                      [App]),
            add_deps(G, Rest);
        false ->
            io:format("  Checking deps for ~p~n", [App]),
            Deps = app_deps(App),
            lists:foreach(
              fun(A) ->
                      digraph:add_vertex(G, A)
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
    ok = application:load(App),

    AppString = atom_to_list(App),
    AppFile = filename:join([AppString, "ebin", AppString ++ ".app"]),
    case code:where_is_file(AppFile) of
        non_existing ->
            error({non_existing, AppFile});
        DotAppFile ->
            DotAppFile
    end.
