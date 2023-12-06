-module(executor).

-include("types.hrl").

-export([execute/1]).

-ifdef(TEST).
-export([
         transform_erlc_opts/1
        ]).
-endif.

-spec execute(request()) -> response().
execute(#{arguments := #{targets_file := ConfigJsonPath}}) ->
    case read_config_file(ConfigJsonPath) of
        {ok, Config} ->
            #{module_index := ModuleIndex,
              code_paths := CodePaths,
              targets := Targets} = Config,

            %% io:format(standard_error, "ERL_LIBS: ~p~n", [os:getenv("ERL_LIBS")]),

            %% io:format(standard_error, "Targets: ~p~n", [Targets]),

            %% io:format(standard_error, "ModuleIndex: ~p~n", [ModuleIndex]),

            %% TreeOut = os:cmd("/usr/local/bin/tree"),
            %% io:format(standard_error, "~s~n", [TreeOut]),

            AnalysisFileContentsTable = ets:new(analysis_file_contents, [set]),

            DestDir = clone_sources(Targets),

            %% io:format(standard_error, "DestDir: ~p~n", [DestDir]),

            %% TreeOut = os:cmd("/usr/local/bin/tree " ++ DestDir),
            %% io:format(standard_error, "~s~n", [TreeOut]),

            %% io:format(standard_error, "CodePaths: ~p~n", [CodePaths]),

            code:add_paths(CodePaths),
            %% io:format(standard_error, "code:get_path() = ~p~n", [code:get_path()]),

            G = app_graph(Targets, ModuleIndex, AnalysisFileContentsTable),

            TargetsWithDeps = add_deps_to_targets(Targets, G),
            % lets update the targets with the deps, before we consume the graph, so that
            % the .app can be generated correctly
            % we might also have to write this to a file so that dialyze rules can use it?
            %% io:format(standard_error, "TargetsWithDeps: ~p~n", [TargetsWithDeps]),

            AppCompileOrder = digraph_tools:consume_to_list(G),

            %% io:format(standard_error, "Compilation Order: ~p~n", [AppCompileOrder]),

            R = lists:foldl(
                  fun
                      (_, {_, {error, _, _} = E}) ->
                          E;
                      (AppName, {FilesSoFar, {ok, Warnings}}) ->
                          #{AppName := Props} = TargetsWithDeps,
                          R = case compile(AppName, Props, DestDir, ModuleIndex, AnalysisFileContentsTable) of
                                  {ok, C, []} ->
                                      {FilesSoFar + C, {ok, Warnings}};
                                  {ok, C, W} ->
                                      {FilesSoFar + C, {ok,
                                                        Warnings ++ [{list_to_atom(AppName), W}]}};
                                  {error, Errors, []} ->
                                      {FilesSoFar, {error,
                                                    {AppName, Errors},
                                                    Warnings}};
                                  {error, Errors, W} ->
                                      {FilesSoFar, {error,
                                                    {AppName, Errors},
                                                    Warnings ++ [{list_to_atom(AppName), W}]}}
                              end,
                          dot_app_file:render(AppName, Props, DestDir),
                          R
                  end, {0, {ok, []}}, AppCompileOrder),
            ets:delete(AnalysisFileContentsTable),
            case R of
                {_, {error, Errors, _}} ->
                    #{exit_code => 1, output => io_lib:format("Failed to compile.~nErrors: ~p~n",
                                                              [Errors])};
                {Count, {ok, []}} ->
                    #{exit_code => 0, output => io_lib:format("Compiled ~p files.~n",
                                                              [Count])};
                {Count, {ok, Warnings}} ->
                    #{exit_code => 0, output => io_lib:format("Compiled ~p files.~nWarnings: ~p~n",
                                                              [Count, Warnings])}
            end;
        {error, Reason} ->
            #{exit_code => 1,
              output => io_lib:format("Could not read ~s: ~p~n", [ConfigJsonPath, Reason])}
    end.

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

conform_code_paths(Dirs) ->
    lists:map(fun binary_to_list/1, Dirs).

-spec conform_config(thoas:json_term()) -> config().
conform_config(#{<<"module_index">> := MI,
                 <<"code_paths">> := CP,
                 <<"targets">> := T}) ->
    #{module_index => conform_index(MI),
      code_paths => conform_code_paths(CP),
      targets => conform_targets(T)}.

-spec read_config_file(string()) -> {ok, config()} | {error, term()}.
read_config_file(ConfigJsonPath) ->
    case file:read_file(ConfigJsonPath) of
        {ok, ConfigJson} ->
            case thoas:decode(ConfigJson) of
                {ok, RawConfig} ->
                    case catch conform_config(RawConfig) of
                        bad_match ->
                            {error, "config did not match expected shape"};
                        Config ->
                            {ok, Config}
                    end;
                E ->
                    E
            end;
        E ->
            E
    end.

clone_app(#{srcs := Srcs, outs := Outs}) ->
    lists:foreach(
      fun (Src) ->
              Module = filename:basename(Src, ".erl"),
              {value, Dest} = lists:search(
                                fun(Out) ->
                                        case filename:basename(Out, ".erl") of
                                            Module -> true;
                                            _ -> false
                                        end
                                end, Outs),
              %% io:format(standard_error, "Copying ~p to ~p~n", [Src, Dest]),
              {ok, _} = file:copy(Src, Dest)
      end, Srcs),
    {value, Beam} = lists:search(
                      fun (Out) ->
                              case filename:extension(Out) of
                                  ".beam" -> true;
                                  _ -> false
                              end
                      end, Outs),
    filename:dirname(filename:dirname(Beam)).

-spec clone_sources(#{string() := target()}) -> string().
clone_sources(Targets) ->
    maps:fold(
      fun
          (_, Props, unknown) ->
              filename:dirname(clone_app(Props));
          (_, Props, DestDir) ->
              DestDir = filename:dirname(clone_app(Props))
      end, unknown, Targets).

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

-spec compile(string(), target(), string(), module_index(), ets:table()) ->
          {ok, ModuleCount :: non_neg_integer(), Warnings :: [string()]} |
          {error, Errors :: [string()], Warnings :: [string()]}.
compile(AppName,
        #{erlc_opts_file := ErlcOptsFile,
          analysis := Analysis,
          analysis_id := Suffix,
          outs := Outs},
        DestDir,
        ModuleIndex,
        AFCT) ->

    ErlcOpts = rules_erlang_compiler:read_flags_file(ErlcOptsFile),
    CompileOpts0 = transform_erlc_opts(ErlcOpts),
    %% OutDir = filename:join([DestDir, AppName, "ebin"]),
    %% CompileOpts = [{outdir, OutDir} | CompileOpts0],
    CompileOpts = [{outdir, "ebin"},
                   {i, "include"},
                   {i, "src"},
                   {i, "../"},
                   return | CompileOpts0],
    io:format(standard_error, "Compiling ~p with ~p~n", [AppName, CompileOpts]),
    CopiedSrcs = lists:filter(
                   fun is_erlang_source/1,
                   Outs),

    G = src_graph(AppName, Suffix, Analysis, CopiedSrcs, ModuleIndex, AFCT),
    %% io:format(standard_error, "~p: ~p~n", [AppName, G]),

    Srcs = digraph_tools:consume_to_list(G),
    %% io:format(standard_error, "~p: ~p~n", [AppName, Srcs]),

    {ok, OldCwd} = file:get_cwd(),
    AppDir = filename:join(DestDir, AppName),
    %% true = code:add_path(filename:join(AppDir, "ebin")),
    ok = file:set_cwd(AppDir),
    %% io:format(standard_error, "Changed to ~p~n", [begin {ok, Cwd} = file:get_cwd(), Cwd end]),
    R = lists:foldl(
          fun
              (Src, {ok, C, Acc}) ->
                  SrcRel = string:prefix(Src, AppDir ++ "/"),
                  io:format(standard_error, "\t~s~n", [SrcRel]),
                  %% TreeOut = os:cmd("/usr/local/bin/tree"),
                  %% io:format(standard_error, "~s~n", [TreeOut]),
                  case compile:file(SrcRel, CompileOpts) of
                      {ok, Module} ->
                          %% io:format(standard_error, "loading ~p~n", [Module]),
                          %% {module, Module} = code:ensure_loaded(Module),
                          ModulePath = filename:join("ebin", atom_to_list(Module) ++ ".beam"),
                          {ok, Contents} = file:read_file(ModulePath),
                          %% io:format(standard_error, "~p SIZE: ~p~n", [ModulePath, filelib:file_size(ModulePath)]),
                          {module, Module} = code:load_binary(Module, ModulePath, Contents),
                          %% io:format(standard_error, "\t\t~p~n",
                          %%           [code:module_status(Module)]),
                          {ok, C + 1};
                      {ok, Module, Warnings} ->
                          lists:foreach(
                            fun (Warning) ->
                                    io:format(standard_error, "~p: ~p~n", [SrcRel, Warning])
                            end, Warnings),
                          ModulePath = filename:join("ebin", atom_to_list(Module) ++ ".beam"),
                          {ok, Contents} = file:read_file(ModulePath),
                          %% io:format(standard_error, "~p SIZE: ~p~n", [ModulePath, filelib:file_size(ModulePath)]),
                          {module, Module} = code:load_binary(Module, ModulePath, Contents),
                          %% io:format(standard_error, "\t\t~p~n",
                          %%           [code:module_status(Module)]),
                          {ok, C + 1, Acc ++ Warnings};
                      {error, Errors, Warnings} ->
                          lists:foreach(
                            fun (Warning) ->
                                    io:format(standard_error, "~p: ~p~n", [SrcRel, Warning])
                            end, Warnings),
                          {error, Errors, Warnings}
                  end;
              (_, E) ->
                  E
          end, {ok, 0, []}, Srcs),
    ok = file:set_cwd(OldCwd),
    R.

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
