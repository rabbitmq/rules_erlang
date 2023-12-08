-module(executor).

-include("types.hrl").

-export([execute/2]).

-ifdef(TEST).
-export([
         transform_erlc_opts/1
        ]).
-endif.

-spec execute(request(), cas:cas()) -> response().
execute(#{arguments := #{targets_file := ConfigJsonPath}, inputs := Inputs}, CAS) ->
    CC = cas:context(CAS, Inputs),
    case config_file:read(ConfigJsonPath) of
        {ok, Config} ->
            #{module_index := ModuleIndex,
              code_paths := CodePaths,
              targets := Targets} = Config,

            io:format(standard_error,
                      "Compiling applications: ~p~n",
                      [maps:keys(Targets)]),

            %% io:format(standard_error, "ERL_LIBS: ~p~n", [os:getenv("ERL_LIBS")]),

            %% io:format(standard_error, "Targets: ~p~n", [Targets]),

            %% io:format(standard_error, "ModuleIndex: ~p~n", [ModuleIndex]),

            %% TreeOut = os:cmd("/usr/local/bin/tree"),
            %% io:format(standard_error, "~s~n", [TreeOut]),

            DestDir = clone_sources(Targets),

            %% io:format(standard_error, "DestDir: ~p~n", [DestDir]),

            %% TreeOut = os:cmd("/usr/local/bin/tree " ++ DestDir),
            %% io:format(standard_error, "~s~n", [TreeOut]),

            %% io:format(standard_error, "CodePaths: ~p~n", [CodePaths]),

            code:add_paths(CodePaths),
            %% io:format(standard_error, "code:get_path() = ~p~n", [code:get_path()]),

            G = app_graph(Targets, ModuleIndex, CC),

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
                      (AppName, {ModulesSoFar, {ok, Warnings}}) ->
                          #{AppName := Props} = TargetsWithDeps,
                          R = case compile(AppName, TargetsWithDeps, DestDir, ModuleIndex, CC) of
                                  {ok, M, []} ->
                                      {ModulesSoFar ++ M, {ok, Warnings}};
                                  {ok, M, W} ->
                                      {ModulesSoFar ++ M, {ok,
                                                           Warnings ++ [{list_to_atom(AppName), W}]}};
                                  {error, Errors, []} ->
                                      {ModulesSoFar, {error,
                                                      {AppName, Errors},
                                                      Warnings}};
                                  {error, Errors, W} ->
                                      {ModulesSoFar, {error,
                                                      {AppName, Errors},
                                                      Warnings ++ [{list_to_atom(AppName), W}]}}
                              end,
                          dot_app_file:render(AppName, Props, DestDir),
                          R
                  end, {[], {ok, []}}, AppCompileOrder),

            {Modules, _} = R,
            lists:foreach(
              fun (Module) ->
                      case code:purge(Module) of
                          true ->
                              code:delete(Module);
                          _ ->
                              ok
                      end
              end, Modules),
            code:del_paths(CodePaths),

            #{hits := AH, misses := AM} = cas:analysis_file_stats(CAS),
            AFCR = 100 * AH / (AH + AM),
            #{hits := BH, misses := BM} = cas:beam_file_stats(CAS),
            BFCR = 100 * BH / (BH + BM),

            io:format(standard_error,
                      "Compiled ~p modules.~n"
                      "Analysis File Cache Hit Rate: ~.1f%~n"
                      "Beam File Cache Hit Rate: ~.1f%~n"
                      "~n",
                      [length(Modules), AFCR, BFCR]),

            case R of
                {_, {error, Errors, _}} ->
                    #{exit_code => 1, output => io_lib:format("Failed to compile.~n"
                                                              "Errors: ~p~n",
                                                              [Errors])};
                {Modules, {ok, []}} ->
                    #{exit_code => 0, output => io_lib:format("Compiled ~p modules.~n"
                                                              "Analysis File Cache Hit Rate: ~.1f%~n"
                                                              "Beam File Cache Hit Rate: ~.1f%~n",
                                                              [length(Modules), AFCR, BFCR])};
                {Modules, {ok, Warnings}} ->
                    #{exit_code => 0, output => io_lib:format("Compiled ~p modules.~n"
                                                              "Analysis File Cache Hit Rate: ~.1f%~n"
                                                              "Beam File Cache Hit Rate: ~.1f%~n"
                                                              "Warnings: ~p~n",
                                                              [length(Modules), AFCR, BFCR, Warnings])}
            end;
        {error, Reason} ->
            #{exit_code => 1,
              output => io_lib:format("Could not read ~s: ~p~n", [ConfigJsonPath, Reason])}
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

-spec compile(string(), #{string() := target()}, string(), module_index(), cas:cas_context()) ->
          {ok, Modules :: [module()], Warnings :: warnings_list()} |
          {error, Errors :: errors_list(), Warnings :: warnings_list()}.
compile(AppName, Targets, DestDir, ModuleIndex, CC) ->
    #{AppName := #{erlc_opts_file := ErlcOptsFile,
                   analysis := Analysis,
                   analysis_id := Suffix,
                   outs := Outs}} = Targets,

    CAS = cas:uncontext(CC),

    ErlcOpts = flags_file:read(ErlcOptsFile),
    CompileOpts0 = transform_erlc_opts(ErlcOpts),
    %% OutDir = filename:join([DestDir, AppName, "ebin"]),
    %% CompileOpts = [{outdir, OutDir} | CompileOpts0],
    CompileOpts = [{outdir, "ebin"},
                   {i, "include"},
                   {i, "src"},
                   {i, "../"},
                   binary,
                   return | CompileOpts0],
    io:format(standard_error, "Compiling ~p with ~p~n", [AppName, CompileOpts]),
    CopiedSrcs = lists:filter(
                   fun is_erlang_source/1,
                   Outs),

    G = src_graph(AppName, Suffix, Analysis, CopiedSrcs, ModuleIndex, CC),
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
              (Src, {ok, Modules, Warnings}) ->
                  SrcRel = string:prefix(Src, AppDir ++ "/"),
                  %% io:format(standard_error, "\t~s~n", [SrcRel]),
                  %% TreeOut = os:cmd("/usr/local/bin/tree"),
                  %% io:format(standard_error, "~s~n", [TreeOut]),
                  ContentsFun = fun () ->
                                        case compile:file(SrcRel, CompileOpts) of
                                            {ok, Module, ModuleBin} ->
                                                {ok, Module, ModuleBin, []};
                                            R ->
                                                R
                                        end
                                end,
                  Contents = case cas:has_inputs(CC) of
                                 true ->
                                     #{AppName := Target} = Targets,
                                     {OriginalSrc, ErlAttrs} = get_analysis(Src, Target, CC),
                                     Deps = deps(OriginalSrc, ErlAttrs, AppName, Targets, ModuleIndex, CC),
                                     Key = beam_file_contents_key(CompileOpts0,
                                                                  Deps,
                                                                  CC),
                                     cas:get_beam_file_contents(Key, ContentsFun, CAS);
                                 false ->
                                     ContentsFun()
                             end,
                  case Contents of
                      {ok, Module, ModuleBin, W} ->
                          ModulePath = filename:join("ebin", atom_to_list(Module) ++ ".beam"),
                          ok = file:write_file(ModulePath, ModuleBin),
                          {module, Module} = code:load_binary(Module, ModulePath, ModuleBin),
                          {ok, Modules ++ [Module], Warnings ++ W};
                      {error, Errors, W} ->
                          {error, Errors, Warnings ++ W}
                  end;
              (_, E) ->
                  E
          end, {ok, [], []}, Srcs),
    ok = file:set_cwd(OldCwd),
    R.

-spec get_analysis(file:name(), target(), cas:cas_context()) -> {file:name(), thoas:json_term()}.
get_analysis(Src, Target, CC) ->
    #{analysis := Analysis, analysis_id := Suffix, srcs := Srcs} = Target,
    SrcBasename = filename:basename(Src),
    {value, OriginalSrc} = lists:search(
                             fun (S) ->
                                     filename:basename(S) == SrcBasename
                             end, Srcs),
    SrcModuleName = filename:basename(OriginalSrc, ".erl"),
    {value, A} = lists:search(
                   fun (AF) ->
                           filename:basename(
                             filename:basename(AF, ".json"),
                             "." ++ Suffix) == SrcModuleName
                   end, Analysis),
    {OriginalSrc, cas:get_analysis_file_contents(A, CC)}.

-spec resolve_module(string(), #{string() := target()}, module_index()) -> ok | {ok, file:name()} | {error, term()}.
resolve_module(ModuleName, Targets, ModuleIndex) when is_list(ModuleName) ->
    case ModuleIndex of
        #{ModuleName := OwnerApp} ->
            #{OwnerApp := OwnerTarget} = Targets,
            #{srcs := OwnerSrcs} = OwnerTarget,
            {value, ModuleSrc} = lists:search(
                                   fun (OwnerSrc) ->
                                           filename:basename(OwnerSrc, ".erl") == ModuleName
                                   end, OwnerSrcs),
            {ok, ModuleSrc};
        _ ->
            %% we also need to track the erlang version through all of this?
            %% maybe not because this escript depends on it too...
            %% but it's here bazel-out/darwin-fastbuild/bin/external/rules_erlang~override~erlang_config~erlang_config/external/otp-external_version

            %% we also need to look for this module in erl_libs...
            case code:which(list_to_atom(ModuleName)) of
                non_existing ->
                    {error, non_existing};
                Path ->
                    case string:prefix(Path, os:getenv("ERLANG_HOME")) of
                        nomatch ->
                            io:format(standard_error, "code:which ~p~n", [Path]),                                             %% check for in ERL_LIBS
                            ok;
                        _ ->
                            %% we could stick the atom in the deps,
                            %% and then write out an extra_apps file?
                            %% for now since this is an erlang dep
                            %% we can ignore it, the dependency
                            %% is covered through the erlang version
                            %% this whole escript depends on
                            ok
                    end
            end
    end.

-spec deps(file:name(), thoas:json_term(),
           string(), #{string() := target()},
           module_index(), cas:cas_context()) -> [file:name()].
deps(OriginalSrc, ErlAttrs, _AppName, Targets, ModuleIndex, _CC) ->
    #{<<"behaviour">> := Behaviours,
      <<"parse_transform">> := Transforms,
      <<"include">> := _Include,
      <<"include_lib">> := _IncludeLib} = ErlAttrs,
    Deps0 = [OriginalSrc],
    Deps = lists:foldl(
             fun (ModuleNameBinary, Acc) ->
                     ModuleName = binary_to_list(ModuleNameBinary),
                     case resolve_module(ModuleName, Targets, ModuleIndex) of
                         ok ->
                             Acc;
                         {ok, ModuleSrc} ->
                             [ModuleSrc | Acc];
                         {error, non_existing} ->
                             io:format(standard_error,
                                       "Ignoring dependency on module ~s in ~p~n",
                                       [ModuleName, OriginalSrc]),
                             Acc
                     end
             end, Deps0, Behaviours ++ Transforms),
    lists:reverse(Deps).

-spec beam_file_contents_key(proplists:proplist(), [string()],
                             cas:cas_context()) -> binary().
beam_file_contents_key(ErlcOpts, Deps, CC) ->
    ErlcOptsBin = term_to_binary(ErlcOpts),
    FilesDigests = cas:digests_in_context(CC, Deps),
    crypto:hash(sha, [ErlcOptsBin | FilesDigests]).

-spec app_graph(#{string() := target()}, module_index(), cas:cas_context()) -> digraph:graph().
app_graph(Targets, ModuleIndex, CC) ->
    G = digraph:new([acyclic]),
    lists:foreach(fun (App) ->
                      digraph:add_vertex(G, App)
                  end, maps:keys(Targets)),
    app_graph(maps:keys(Targets), Targets, ModuleIndex, G, CC).

app_graph([], _, _, G, _) ->
    G;
app_graph([App | Rest], Targets, ModuleIndex, G, CC) ->
    #{App := #{analysis := AnalysisFiles}} = Targets,
    app_deps(App, AnalysisFiles, ModuleIndex, G, CC),
    app_graph(Rest, Targets, ModuleIndex, G, CC).

app_deps(_, [], _, _, _) ->
    ok;
app_deps(AppName, [AnalysisFile | Rest], ModuleIndex, G, CC) ->
    ErlAttrs = cas:get_analysis_file_contents(AnalysisFile, CC),

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
    app_deps(AppName, Rest, ModuleIndex, G, CC).

-spec src_graph(string(), string(), [string()], [string()], #{string() := string()}, cas:cas_context()) -> digraph:graph().
src_graph(AppName, Suffix, Analysis, Srcs, ModuleIndex, CC) ->
    G = digraph:new([acyclic]),
    lists:foreach(fun (Src) ->
                          digraph:add_vertex(G, Src)
                  end, Srcs),
    src_graph(AppName, Suffix, Analysis, Srcs, ModuleIndex, G, CC).

src_graph(_, _, [], _, _, G, _) ->
    G;
src_graph(AppName, Suffix, [A | Rest], Srcs, ModuleIndex, G, CC) ->
    ModuleName = filename:basename(filename:basename(A, ".json"), "." ++ Suffix),
    %% io:format(standard_error, "Checking deps for ~p~n", [ModuleName]),
    {value, Src} = lists:search(
                     fun (S) ->
                             filename:basename(S, ".erl") == ModuleName
                     end, Srcs),
    ErlAttrs = cas:get_analysis_file_contents(A, CC),
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

    src_graph(AppName, Suffix, Rest, Srcs, ModuleIndex, G, CC).

is_erlang_source(F) ->
    case filename:extension(F) of
        ".erl" ->
            true;
        _ ->
            false
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
