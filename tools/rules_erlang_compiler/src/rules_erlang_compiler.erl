-module(rules_erlang_compiler).

-export([main/1]).

-ifdef(TEST).
-export([
         consume_to_list/1,
         render_dot_app_file/3,
         transform_erlc_opts/1
        ]).
-endif.

% -type input() :: #{path := string(), digest := string()}.
% -type request_args() :: #{in := string(),
%                           out := string(),
%                           macros := [macro()],
%                           includes := [include()]}.
% -type request() :: #{arguments := request_args(),
%                      inputs := [input()],
%                      request_id => integer()}.

% -type response() :: #{exit_code := integer(), output := string()}.

-type target() :: #{path := string(),
                    erlc_opts_file := string(),
                    app_src := string() | null,
                    srcs := [string()],
                    analysis := [string()],
                    analysis_id := string(),
                    outs := [string()]}.

-type config() :: #{dest_dir := string(),
                    module_index := #{string() := string()},
                    targets := #{string() := target()}}.

-spec main([string()]) -> no_return().
main([ConfigJsonPath]) ->
    {ok, ConfigJson} = file:read_file(ConfigJsonPath),
    {ok, RawConfig} = thoas:decode(ConfigJson),
    Config = conform_config(RawConfig),

    #{dest_dir := DestDir,
      module_index := ModuleIndex,
      targets := Targets} = Config,

    %% io:format(standard_error, "ERL_LIBS: ~p~n", [os:getenv("ERL_LIBS")]),

    %% io:format(standard_error, "Targets: ~p~n", [Targets]),

    %% io:format(standard_error, "ModuleIndex: ~p~n", [ModuleIndex]),

    %% io:format(standard_error, "DestDir: ~p~n", [DestDir]),

    %% TreeOut = os:cmd("/usr/local/bin/tree"),
    %% io:format(standard_error, "~s~n", [TreeOut]),

    AnalysisFileContentsTable = ets:new(analysis_file_contents, [set]),

    ok = clone_sources(DestDir, Targets),

    %% TreeOut = os:cmd("/usr/local/bin/tree " ++ DestDir),
    %% io:format(standard_error, "~s~n", [TreeOut]),

    %% it seems we would need to add each app individually after compilation,
    %% which would be fine, but maybe we just add DestDir to ERL_LIBS?
    %% true = code:add_path(DestDir),
    %% io:format(standard_error, "code:get_path() = ~p~n", [code:get_path()]),

    G = app_graph(Targets, ModuleIndex, AnalysisFileContentsTable),

    TargetsWithDeps = add_deps_to_targets(Targets, G),
    % lets update the targets with the deps, before we consume the graph, so that
    % the .app can be generated correctly
    % we might also have to write this to a file so that dialyze rules can use it?
    %% io:format(standard_error, "TargetsWithDeps: ~p~n", [TargetsWithDeps]),

    AppCompileOrder = consume_to_list(G),

    %% io:format(standard_error, "Compilation Order: ~p~n", [AppCompileOrder]),

    lists:foreach(
      fun (AppName) ->
              #{AppName := Props} = TargetsWithDeps,
              compile(AppName, Props, DestDir, ModuleIndex, AnalysisFileContentsTable),
              render_dot_app_file(AppName, Props, DestDir)
      end, AppCompileOrder),
    ets:delete(AnalysisFileContentsTable),
    ok;
main(_) ->
    exit(1).

conform_target(#{<<"path">> := Path,
                 <<"erlc_opts_file">> := ErlcOptsFile,
                 <<"app_src">> := AppSrc,
                 <<"srcs">> := Srcs,
                 <<"analysis">> := Analysis,
                 <<"analysis_id">> := AnalysisId,
                 <<"outs">> := Outs}) ->
    #{path => binary_to_list(Path),
      erlc_opts_file => binary_to_list(ErlcOptsFile),
      app_src => case AppSrc of
                     null -> null;
                     _ -> binary_to_list(AppSrc)
                 end,
      srcs => lists:map(fun binary_to_list/1, Srcs),
      analysis => lists:map(fun binary_to_list/1, Analysis),
      analysis_id => binary_to_list(AnalysisId),
      outs => lists:map(fun binary_to_list/1, Outs)}.

conform_targets(Targets) ->
    maps:fold(
      fun (K, V, Acc) ->
              Acc#{binary_to_list(K) => conform_target(V)}
      end, #{}, Targets).

conform_index(ModuleIndex) ->
    maps:fold(
      fun (K, V, Acc) ->
              % maybe these should be atoms?
              Acc#{binary_to_list(K) => binary_to_list(V)}
      end, #{}, ModuleIndex).

-spec conform_config(thoas:json_term()) -> config().
conform_config(#{<<"dest_dir">> := DD, <<"module_index">> := MI, <<"targets">> := T}) ->
    #{dest_dir => binary_to_list(DD),
      module_index => conform_index(MI),
      targets => conform_targets(T)}.

clone_app(DestDir, AppName, #{path := AppPath, srcs := Srcs, outs := Outs}) ->
    lists:foreach(
      fun (Src) ->
              RP = case AppPath of
                       "" -> Src;
                       _ -> string:prefix(Src, AppPath ++ "/")
                   end,
              %% io:format(standard_error, "App: ~p, Src: ~p, AppPath: ~p, RP: ~p~n", [AppName, Src, AppPath, RP]),
              Dest = filename:join([DestDir, AppName, RP]),
              true = lists:member(Dest, Outs),
              %% io:format(standard_error, "Copying ~p to ~p~n", [Src, Dest]),
              {ok, _} = file:copy(Src, Dest)
      end, Srcs).

clone_sources(DestDir, Targets) ->
    maps:foreach(
      fun (AppName, Props) ->
              ok = clone_app(DestDir, AppName, Props)
      end, Targets).

app_graph(Targets, ModuleIndex, T) ->
    G = digraph:new([acyclic]),
    lists:foreach(fun (App) ->
                      digraph:add_vertex(G, App)
                  end, maps:keys(Targets)),
    app_graph(maps:keys(Targets), Targets, ModuleIndex, G, T).

app_graph([], _, _, G, _) ->
    G;
app_graph([App | Rest], Targets, ModuleIndex, G, T) ->
    #{App := #{analysis := AnalysisFiles}} = Targets,
    app_deps(App, AnalysisFiles, ModuleIndex, G, T),
    app_graph(Rest, Targets, ModuleIndex, G, T).

app_deps(_, [], _, _, _) ->
    ok;
app_deps(AppName, [AnalysisFile | Rest], ModuleIndex, G, T) ->
    ErlAttrs = get_analysis_file_contents(AnalysisFile, T),

    #{<<"behaviour">> := Behaviours,
      <<"parse_transform">> := Transforms,
      <<"include_lib">> := IncludeLibs} = ErlAttrs,
    lists:foreach(
      fun (ModuleBin) ->
              ModuleString = binary_to_list(ModuleBin),
              case ModuleIndex of
                  #{ModuleString := OtherApp} when OtherApp =/= AppName ->
                      io:format(standard_error, "app_graph: adding edge ~p <- ~p~n", [OtherApp, AppName]),
                      digraph:add_edge(G, OtherApp, AppName);
                  _ ->
                      ok
              end
      end, Behaviours ++ Transforms),
    lists:foreach(
      fun (IncludeLibBin) ->
              IncludeLib = binary_to_list(IncludeLibBin),
              case string:split(IncludeLib, "/") of
                  [OtherApp, _] when OtherApp =/= AppName ->
                      case lists:member(OtherApp, maps:values(ModuleIndex)) of
                          true ->
                              io:format(standard_error, "app_graph: adding hdr edge ~p <- ~p~n", [OtherApp, AppName]),
                              digraph:add_edge(G, OtherApp, AppName);
                          false ->
                              ok
                      end
              end
      end, IncludeLibs),
    app_deps(AppName, Rest, ModuleIndex, G, T).

-spec src_graph(string(), string(), [string()], [string()], #{string() := string()}, ets:table()) -> digraph:graph().
src_graph(AppName, Suffix, Analysis, Srcs, ModuleIndex, T) ->
    G = digraph:new([acyclic]),
    lists:foreach(fun (Src) ->
                          digraph:add_vertex(G, Src)
                  end, Srcs),
    src_graph(AppName, Suffix, Analysis, Srcs, ModuleIndex, G, T).

src_graph(_, _, [], _, _, G, _) ->
    G;
src_graph(AppName, Suffix, [A | Rest], Srcs, ModuleIndex, G, T) ->
    ModuleName = filename:basename(filename:basename(A, ".json"), "." ++ Suffix),
    %% io:format(standard_error, "Checking deps for ~p~n", [ModuleName]),
    {value, Src} = lists:search(
                     fun (S) ->
                             filename:basename(S, ".erl") == ModuleName
                     end, Srcs),
    ErlAttrs = get_analysis_file_contents(A, T),
    #{<<"behaviour">> := Behaviours,
      <<"parse_transform">> := Transforms} = ErlAttrs,
    %% we can safely ignore include_lib here, as we compile applications
    %% as units for now, and will always recompile the app when any of
    %% its files change
    lists:foreach(
      fun (ModuleBin) ->
              ModuleString = binary_to_list(ModuleBin),
              %% io:format(standard_error, "BehaviorOrXform: ~p~n", [ModuleString]),
              %% io:format(standard_error, "ModuleIndex: ~p => ~p~n", [ModuleString, maps:find(ModuleString, ModuleIndex)]),

              case ModuleIndex of
                  #{ModuleString := AppName} ->
                      {value, MS} = lists:search(
                                      fun (S) ->
                                              case filename:basename(S, ".erl") of
                                                  ModuleString -> true;
                                                  _ -> false
                                              end
                                      end, Srcs),
                      %% io:format(standard_error, "src_graph: adding edge ~p <- ~p~n", [MS, Src]),
                      digraph:add_edge(G, MS, Src);
                  _ ->
                      % if this is to a built-in app, we can handle extra_app automatically?
                      %% io:format(standard_error, "src_graph: ignoring ~p <- ~p~n", [ModuleString, AppName]),
                      ok
              end
      end, Behaviours ++ Transforms),

    src_graph(AppName, Suffix, Rest, Srcs, ModuleIndex, G, T).

is_erlang_source(F) ->
    case filename:extension(F) of
        ".erl" ->
            true;
        _ ->
            false
    end.

compile(AppName,
        #{erlc_opts_file := ErlcOptsFile,
          analysis := Analysis,
          analysis_id := Suffix,
          outs := Outs},
        DestDir,
        ModuleIndex,
        AFCT) ->

    ErlcOpts = read_flags_file(ErlcOptsFile),
    CompileOpts0 = transform_erlc_opts(ErlcOpts),
    %% OutDir = filename:join([DestDir, AppName, "ebin"]),
    %% CompileOpts = [{outdir, OutDir} | CompileOpts0],
    CompileOpts = [{outdir, "ebin"},
                   {i, "include"},
                   {i, "src"},
                   {i, "../"},
                   report | CompileOpts0],
    io:format(standard_error, "Compiling ~p with ~p~n", [AppName, CompileOpts]),
    CopiedSrcs = lists:filter(
                   fun is_erlang_source/1,
                   Outs),

    G = src_graph(AppName, Suffix, Analysis, CopiedSrcs, ModuleIndex, AFCT),
    %% io:format(standard_error, "~p: ~p~n", [AppName, G]),

    Srcs = consume_to_list(G),
    %% io:format(standard_error, "~p: ~p~n", [AppName, Srcs]),

    {ok, OldCwd} = file:get_cwd(),
    AppDir = filename:join(DestDir, AppName),
    %% true = code:add_path(filename:join(AppDir, "ebin")),
    ok = file:set_cwd(AppDir),
    %% io:format(standard_error, "Changed to ~p~n", [begin {ok, Cwd} = file:get_cwd(), Cwd end]),
    lists:foreach(
      fun (Src) ->
              SrcRel = string:prefix(Src, AppDir ++ "/"),
              io:format(standard_error, "\t~s~n", [SrcRel]),
              {ok, Module} = compile:file(SrcRel, CompileOpts),
              %% TreeOut = os:cmd("/usr/local/bin/tree"),
              %% io:format(standard_error, "~s~n", [TreeOut]),
              %% io:format(standard_error, "loading ~p~n", [Module]),
              %% {module, Module} = code:ensure_loaded(Module),
              ModulePath = filename:join("ebin", atom_to_list(Module) ++ ".beam"),
              {ok, Contents} = file:read_file(ModulePath),
              %% io:format(standard_error, "~p SIZE: ~p~n", [ModulePath, filelib:file_size(ModulePath)]),
              {module, Module} = code:load_binary(Module, ModulePath, Contents),
              %% io:format(standard_error, "\t\t~p~n",
              %%           [code:module_status(Module)]),
              ok
      end, Srcs),
    ok = file:set_cwd(OldCwd).

-spec consume_to_list(digraph:graph()) -> [digraph:vertex()].
consume_to_list(G) ->
    consume_to_list(G, []).

consume_to_list(G, Acc) ->
    case lists:sort(digraph:vertices(G)) of
        [] ->
            digraph:delete(G),
            Acc;
        [V | _]  ->
            R = find_root(G, V),
            digraph:del_vertex(G, R),
            consume_to_list(G, Acc ++ [R])
    end.

find_root(G, V) ->
    Edges = lists:sort(
              fun (E1, E2) ->
                      {E1, P1, V, _} = digraph:edge(G, E1),
                      {E2, P2, V, _} = digraph:edge(G, E2),
                      case lists:sort([P1, P2]) of
                          [P1, P2] -> true;
                          [P2, P1] -> false
                      end
              end,
              digraph:in_edges(G, V)),
    case Edges of
        [] ->
            V;
        [E | _] ->
            {E, P, V, _} = digraph:edge(G, E),
            find_root(G, P)
    end.

-spec string_to_term(string()) -> term().
string_to_term(S) ->
    {ok, Tokens, _} = erl_scan:string(S ++ "."),
    {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

-spec transform_erlc_opts([string()]) -> [compile:option()].
transform_erlc_opts(ErlcOpts) ->
    lists:map(
      fun
          ("-Werror") ->
              warnings_as_errors;
          ("+" ++ Term) ->
              list_to_atom(Term);
          ("-D" ++ Macro) ->
              {d, list_to_atom(Macro)};
          ("'-D" ++ Macro) ->
              M = string:strip(Macro, right, $'),
              case string:split(M, "=") of
                  [A, V] ->
                      {d, list_to_atom(A), string_to_term(V)}
              end
      end, ErlcOpts).

-spec get_analysis_file_contents(string(), ets:table()) -> thoas:json_term().
get_analysis_file_contents(F, Table) ->
    case ets:lookup(Table, F) of
        [{F, ErlAttrs}] ->
            ErlAttrs;
        [] ->
            {ok, Contents} = file:read_file(F),
            {ok, ErlAttrs} = thoas:decode(Contents),
            true = ets:insert_new(Table, {F, ErlAttrs}),
            ErlAttrs
    end.

-spec render_dot_app_file(string(), target(), string()) -> ok.
render_dot_app_file(AppNameString,
                    #{app_src := AppSrc, outs := Outs} = Target,
                    DestDir) ->
    AppName = list_to_atom(AppNameString),
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
    Dest = filename:join([DestDir, AppName, "ebin", AppNameString ++ ".app"]),
    ok = file:write_file(Dest,
                         io_lib:format("~tp.~n", [{application,
                                                   AppName,
                                                   Props}])).

add_deps_to_targets(Targets, G) ->
    add_deps_to_targets(Targets, digraph:vertices(G), G).

add_deps_to_targets(Targets, [], _ ) ->
    Targets;
add_deps_to_targets(Targets0, [V | Rest], G) ->
    Edges = digraph:edges(G, V),
    %% io:format(standard_error, "V: ~p, Edges: ~p~n", [V, Edges]),
    Targets = add_deps_to_targets(Targets0, V, Edges, G),
    add_deps_to_targets(Targets, Rest, G).

add_deps_to_targets(Targets, _, [], _) ->
    Targets;
add_deps_to_targets(Targets, V, [E | Rest], G) ->
    {E, Dep, Dependent, _ } = digraph:edge(G, E),
    #{Dependent := Target0} = Targets,
    %% io:format(standard_error, "~p <- ~p~n", [Dep, Dependent]),
    Target = maps:update_with(deps,
                              fun (S) ->
                                      sets:add_element(Dep, S)
                              end,
                              sets:new(), Target0),
    %% io:format(standard_error, "Target: ~p~n", [Target]),
    add_deps_to_targets(Targets#{Dependent := Target}, V, Rest, G).

read_flags_file(Path) ->
    {ok, D} = file:open(Path, [read]),
    try all_lines(D)
    after file:close(D)
    end.

all_lines(D) ->
    case io:get_line(D, "") of
        eof -> [];
        L -> [string:trim(L, trailing, "\n") | all_lines(D)]
    end.
