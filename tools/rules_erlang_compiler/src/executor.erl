-module(executor).

-include_lib("erl_attrs_to_json/include/erl_attrs_to_json.hrl").

-include("types.hrl").

-export([execute/2]).

-spec execute(request(), cas:cas()) -> response().
execute(#{arguments := #{targets_file := ConfigJsonPath}, inputs := Inputs}, CAS) ->
    case config_file:read(ConfigJsonPath) of
        {ok, Config} ->
            #{module_index := ModuleIndex,
              code_paths := CodePaths,
              targets := Targets} = Config,

            CodePathApps = lists:map(
                             fun (CodePath) ->
                                     list_to_atom(
                                       filename:basename(
                                         filename:dirname(CodePath)))
                             end, CodePaths),

            io:format(standard_error,
                      "Available applications on code path: ~p~n"
                      "Compiling applications: ~p~n",
                      [CodePathApps, maps:keys(Targets)]),

            %% io:format(standard_error, "Targets: ~p~n", [Targets]),

            %% io:format(standard_error, "ModuleIndex: ~p~n", [ModuleIndex]),

            %% TreeOut = os:cmd("/usr/local/bin/tree"),
            %% io:format(standard_error, "~s~n", [TreeOut]),

            {DestDir, MappedInputs} = clone_sources(Targets, Inputs),

            CC = cas:context(CAS, MappedInputs),

            %% io:format(standard_error, "DestDir: ~p~n", [DestDir]),

            %% TreeOut = os:cmd("/usr/local/bin/tree " ++ DestDir),
            %% io:format(standard_error, "~s~n", [TreeOut]),

            %% io:format(standard_error, "CodePaths: ~p~n", [CodePaths]),

            AbsCodePaths = lists:map(fun filename:absname/1, CodePaths),

            lists:foreach(
              fun (CodePath) ->
                      true = code:add_path(CodePath)
              end, AbsCodePaths),
            %% io:format(standard_error, "code:get_path() = ~p~n", [code:get_path()]),

            TargetsWithCompileOpts = add_compile_opts_and_dest_dir_to_targets(DestDir, Targets),

            G = app_graph(TargetsWithCompileOpts, ModuleIndex, CC),

            TargetsWithDeps = add_deps_to_targets(TargetsWithCompileOpts, G),
            % lets update the targets with the deps, before we consume the graph, so that
            % the .app can be generated correctly
            % we might also have to write this to a file so that dialyze rules can use it?
            %% io:format(standard_error, "TargetsWithDeps: ~p~n", [TargetsWithDeps]),

            AppCompileOrder = digraph_tools:consume_to_list(G),

            %% io:format(standard_error, "Compilation Order: ~p~n", [AppCompileOrder]),

            R = lists:foldl(
                  fun
                      (_, {_, {error, _, _}} = E) ->
                          E;
                      (AppName, {ModulesSoFar, {ok, Warnings}}) ->
                          #{AppName := Props} = TargetsWithDeps,
                          R = case compile(AppName, TargetsWithDeps, DestDir, ModuleIndex, CC) of
                                  {ok, M, []} ->
                                      {ModulesSoFar ++ M, {ok, Warnings}};
                                  {ok, M, W} ->
                                      {ModulesSoFar ++ M, {ok,
                                                           Warnings ++ [{AppName, W}]}};
                                  {error, Errors, []} ->
                                      {ModulesSoFar, {error,
                                                      {AppName, Errors},
                                                      Warnings}};
                                  {error, Errors, W} ->
                                      {ModulesSoFar, {error,
                                                      {AppName, Errors},
                                                      Warnings ++ [{AppName, W}]}}
                              end,
                          dot_app_file:render(AppName, Props, DestDir),
                          R
                  end, {[], {ok, []}}, AppCompileOrder),

            {Modules, _} = R,
            lists:foreach(
              fun
                  (thoas) ->
                      ok;
                  (thoas_decode) ->
                      ok;
                  (Module) ->
                      case code:delete(Module) of
                          true ->
                              ok;
                          _ ->
                              io:format(standard_error,
                                        "Could not delete module ~p.~n",
                                        [Module])
                      end
              end, Modules),
            code:del_paths(AbsCodePaths),

            #{hits := AH, misses := AM} = cas:src_analysis_stats(CAS),
            ACR = 100 * AH / (AH + AM),
            #{hits := BH, misses := BM} = cas:beam_file_stats(CAS),
            BFCR = case BH + BM of
                       0 -> 0.0;
                       _ -> 100 * BH / (BH + BM)
                   end,

            io:format(standard_error,
                      "Compiled ~p modules.~n"
                      "Analysis Cache Hit Rate: ~.1f%~n"
                      "Beam File Cache Hit Rate: ~.1f%~n"
                      "~n",
                      [length(Modules), ACR, BFCR]),

            case R of
                {_, {error, Errors, _}} ->
                    #{exit_code => 1, output => io_lib:format("Failed to compile.~n"
                                                              "Errors: ~p~n",
                                                              [Errors])};
                {Modules, {ok, []}} ->
                    #{exit_code => 0, output => io_lib:format("Compiled ~p modules.~n"
                                                              "Analysis Cache Hit Rate: ~.1f%~n"
                                                              "Beam File Cache Hit Rate: ~.1f%~n",
                                                              [length(Modules), ACR, BFCR])};
                {Modules, {ok, Warnings}} ->
                    #{exit_code => 0, output => io_lib:format("Compiled ~p modules.~n"
                                                              "Analysis Cache Hit Rate: ~.1f%~n"
                                                              "Beam File Cache Hit Rate: ~.1f%~n"
                                                              "Warnings: ~p~n",
                                                              [length(Modules), ACR, BFCR, Warnings])}
            end;
        {error, Reason} ->
            #{exit_code => 1,
              output => io_lib:format("Could not read ~s: ~p~n", [ConfigJsonPath, Reason])}
    end.

clone_app(#{srcs := Srcs, outs := Outs}, Inputs, MappedInputs0) ->
    MappedInputs = lists:foldl(
                     fun (Src, MI) ->
                             #{Src := Digest} = Inputs,
                             Module = filename:basename(Src, ".erl"),
                             {value, Dest} = lists:search(
                                               fun(Out) ->
                                                       case filename:basename(Out, ".erl") of
                                                           Module -> true;
                                                           _ -> false
                                                       end
                                               end, Outs),
                             %% io:format(standard_error, "Copying ~p to ~p~n", [Src, Dest]),
                             {ok, _} = file:copy(Src, Dest),
                             MI#{Dest => Digest}
                     end, MappedInputs0, Srcs),
    {value, Beam} = lists:search(
                      fun (Out) ->
                              case filename:extension(Out) of
                                  ".beam" -> true;
                                  _ -> false
                              end
                      end, Outs),
    {filename:dirname(filename:dirname(Beam)), MappedInputs}.

-spec clone_sources(#{atom() := target()}, #{file:name() := binary()}) -> {string(), #{file:name() := binary()}}.
clone_sources(Targets, Inputs) ->
    maps:fold(
      fun
          (_, Props, {unknown, MappedInputs0}) ->
                  {AppDest, MappedInputs} = clone_app(Props, Inputs, MappedInputs0),
                  {filename:dirname(AppDest), MappedInputs};
          (_, Props, {DestDir, MappedInputs0}) ->
                  {AppDest, MappedInputs} = clone_app(Props, Inputs, MappedInputs0),
                  DestDir = filename:dirname(AppDest),
                  {DestDir, MappedInputs}
      end, {unknown, #{}}, Targets).

-spec compile(atom(), #{atom() := target()}, string(), module_index(), cas:cas_context()) ->
          {ok, Modules :: [module()], Warnings :: warnings_list()} |
          {error, Errors :: errors_list(), Warnings :: warnings_list()}.
compile(AppName, Targets, DestDir, ModuleIndex, CC) ->
    #{AppName := #{compile_opts := CompileOpts0,
                   outs := Outs}} = Targets,

    CAS = cas:uncontext(CC),

    CompileOpts = [{outdir, "ebin"},
                   binary,
                   return | CompileOpts0],
    io:format(standard_error, "Compiling ~p with ~p~n", [AppName, CompileOpts]),

    IncludePaths = lists:filtermap(
                     fun
                         ({i, P}) ->
                             {true, P};
                         (_) ->
                             false
                     end, CompileOpts),

    CopiedSrcs = lists:filter(
                   fun is_erlang_source/1,
                   Outs),

    G = src_graph(AppName, CopiedSrcs, CompileOpts0, ModuleIndex, CC),
    %% io:format(standard_error, "~p: ~p~n", [AppName, G]),

    OrderedCopiedSrcs = digraph_tools:consume_to_list(G),
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
                                     ErlAttrs = get_analysis(Src, CompileOpts0, CC),
                                     case deps(Src, ErlAttrs, IncludePaths, AppName, Targets, ModuleIndex, CC) of
                                         {ok, Deps, []} ->
                                             Key = beam_file_contents_key(CompileOpts0,
                                                                          Deps,
                                                                          CC),
                                             cas:get_beam_file_contents(Key, ContentsFun, CAS);
                                         {ok, Deps, DepsWarnings} ->
                                             Key = beam_file_contents_key(CompileOpts0,
                                                                          Deps,
                                                                          CC),
                                             case cas:get_beam_file_contents(Key, ContentsFun, CAS) of
                                                 {ok, M, MB, CW} ->
                                                     {ok, M, MB, [{Src, DepsWarnings} | CW]};
                                                 {error, E, CW} ->
                                                     {error, E, [{Src, DepsWarnings} | CW]}
                                             end
                                     end;
                                 false ->
                                     ContentsFun()
                             end,
                  case Contents of
                      {ok, Module, ModuleBin, W} ->
                          ModulePath = filename:join("ebin", atom_to_list(Module) ++ ".beam"),
                          ok = file:write_file(ModulePath, ModuleBin),
                          {ok, Cwd} = file:get_cwd(),
                          {module, Module} = code:load_binary(Module,
                                                              filename:join(Cwd, ModulePath),
                                                              ModuleBin),
                          {ok, Modules ++ [Module], Warnings ++ W};
                      {error, Errors, W} ->
                          {error, Errors, Warnings ++ W}
                  end;
              (_, E) ->
                  E
          end, {ok, [], []}, OrderedCopiedSrcs),
    ok = file:set_cwd(OldCwd),
    R.

-spec get_analysis(file:name(), [compile:option()], cas:cas_context()) -> src_analysis().
get_analysis(Src, CompileOpts, CC) ->
    ContentsFun = fun() ->
                          Macros = lists:filtermap(
                                     fun
                                         ({d, D}) ->
                                             {true, D};
                                         ({d, D, V}) ->
                                             {true, {D, V}};
                                         (_) ->
                                             false
                                     end, CompileOpts),
                          Includes = lists:filtermap(
                                       fun
                                           ({i, I}) ->
                                               {true, I};
                                           (_) ->
                                               false
                                       end, CompileOpts),
                          erl_attrs_to_json:parse(Src, Macros, Includes)
                  end,
    {ok, ErlAttrs} = case cas:has_inputs(CC) of
                         true ->
                             ErlcOptsBin = term_to_binary(CompileOpts),
                             Digest = cas:digest_in_context(CC, Src),
                             Key = crypto:hash(sha, [ErlcOptsBin, Digest]),
                             CAS = cas:uncontext(CC),
                             cas:get_analysis(Key, ContentsFun, CAS);
                         false ->
                             ContentsFun()
                     end,
    ErlAttrs.

-spec resolve_module(module(), #{atom() := target()}, module_index()) -> ok | {ok, file:name()} | {warning, term()}.
resolve_module(Module, Targets, ModuleIndex) ->
    case ModuleIndex of
        #{Module := OwnerApp} ->
            #{OwnerApp := OwnerTarget} = Targets,
            #{outs := OwnerOuts} = OwnerTarget,
            OwnerSrcs = lists:filter(fun is_erlang_source/1, OwnerOuts),
            {value, ModuleSrc} = lists:search(
                                   fun (OwnerSrc) ->
                                           list_to_atom(filename:basename(OwnerSrc, ".erl")) == Module
                                   end, OwnerSrcs),
            {ok, ModuleSrc};
        _ ->
            %% we also need to track the erlang version through all of this?
            %% maybe not because this escript depends on it too...
            %% but it's here bazel-out/darwin-fastbuild/bin/external/rules_erlang~override~erlang_config~erlang_config/external/otp-external_version

            %% we also need to look for this module in erl_libs...
            EL = code:ensure_loaded(Module),
            io:format(standard_error,
                      "code:ensure_loaded(~p) => ~p~n",
                      [Module, EL]),
            %% case Module of
            %%     gen_batch_server ->
            %%         LA = code:load_abs("bazel-out/darwin_x86_64-fastbuild/bin/external/rules_erlang~override~erlang_package~erlang_packages/deps/gen_batch_server/ebin/gen_batch_server.beam"),
            %%         io:format(standard_error,
            %%                   "code:load_abs(...) => ~p~n",
            %%                   [LA]);
            %%     _ ->
            %%         ok
            %% end,
            case code:which(Module) of
                non_existing ->
                    io:format(standard_error,
                              "Could not locate source for module ~p.~n",
                              [Module]),
                    {warning, {module_not_found, Module}};
                Path ->
                    case string:prefix(Path, os:getenv("ERLANG_HOME")) of
                        nomatch ->
                            io:format(standard_error, "code:which ~p~n", [Path]),
                            %% check for in ERL_LIBS
                            ok;
                        _ ->
                            %% we could stick the atom in the deps,
                            %% and then write out an extra_apps file?
                            %% we could also do so in app_graph function...
                            %% for now since this is an erlang dep
                            %% we can ignore it, the dependency
                            %% is covered through the erlang version
                            %% this whole escript depends on
                            ok
                    end
            end
    end.

join_resolving_relative(Name1, Name2) ->
    case string:prefix(Name2, "../") of
        nomatch ->
            filename:join(Name1, Name2);
        R ->
            join_resolving_relative(filename:dirname(Name1), R)
    end.

-spec resolve_include(file:name(), string(), [string()], atom(), #{atom() := target()}) ->
          ok | {ok, file:name()} | {warning, term()}.
resolve_include(Src, Include, IncludePaths, AppName, Targets) ->
    case string:prefix(Include, os:getenv("ERLANG_HOME")) of
        nomatch ->
            %% Note: since we are working with OriginalSrc, arguably
            %% we should find the copied src, resolve against that,
            %% then go find that original. However, the analysis was
            %% was actually done againts the OriginalSrc, so it's
            %% probably fine
            #{AppName := Target} = Targets,
            #{dest_dir := DestDir, outs := Outs} = Target,
            Hdrs = lists:filter(fun is_erlang_header/1, Outs),
            SearchPaths = [DestDir,
                           filename:dirname(Src) |
                           lists:map(fun (IncludePath) ->
                                             join_resolving_relative(DestDir, IncludePath)
                                     end, IncludePaths)],

            ExpectedPaths = lists:map(
                              fun (SearchPath) ->
                                      case string:prefix(Include, "../") of
                                          nomatch ->
                                              filename:join(SearchPath, Include);
                                          R ->
                                              join_resolving_relative(SearchPath, R)
                                      end
                              end, SearchPaths),

            case lists:search(fun (Hdr) ->
                                      lists:member(Hdr, ExpectedPaths)
                              end, Hdrs) of
                {value, IncludeSrc} ->
                    {ok, IncludeSrc};
                _ ->
                    io:format(standard_error,
                              "Could not locate source for ~p relative to ~p~n"
                              "    expected at ~p~n"
                              "    in ~p~n",
                              [Include, Src, ExpectedPaths, Hdrs]),
                    {warning, {include_not_found, Src, Include}}
            end;
        _ ->
            %% it's unclear to me, if an app loads an otp header, should
            %% the otp app be part of extra_apps? I think not, as that
            %% would appear to be more of a runtime thing
            ok
    end.

-spec deps(file:name(), src_analysis(), [string()],
           atom(), #{atom() := target()},
           module_index(), cas:cas_context()) -> {ok, [file:name()], [term()]}.
deps(Src, ErlAttrs, IncludePaths, AppName, Targets, ModuleIndex, _CC) ->
    #{behaviour := Behaviours,
      parse_transform := Transforms,
      include := Includes,
      include_lib := IncludeLibs} = ErlAttrs,
    R0 = {ok, [Src], []},
    R1 = lists:foldl(
           fun
               (_, {error, _} = E) ->
                   E;
               (Module, {ok, Deps, Warnings}) ->
                   case resolve_module(Module, Targets, ModuleIndex) of
                       ok ->
                           {ok, Deps, Warnings};
                       {ok, ModuleSrc} ->
                           {ok, [ModuleSrc | Deps], Warnings};
                       {warning, W} ->
                           {ok, Deps, [W | Warnings]}
                   end
           end, R0, Behaviours ++ Transforms),
    R2 = lists:foldl(
           fun
               (_, {error, _} = E) ->
                   E;
               (IncludeBin, {ok, Deps, Warnings}) ->
                   Include = binary_to_list(IncludeBin),
                   case resolve_include(Src, Include, IncludePaths, AppName, Targets) of
                       ok ->
                           {ok, Deps, Warnings};
                       {ok, IncludeSrc} ->
                           {ok, [IncludeSrc | Deps], Warnings};
                       {warning, W} ->
                           {ok, Deps, [W | Warnings]}
                   end
           end, R1, Includes),
    R = lists:foldl(
          fun
              (_, {error, _} = E) ->
                  E;
              (IncludeLib, {ok, _Deps, _Warnings} = Acc) ->
                  io:format(standard_error,
                            "IncludeLib: ~p~n", [IncludeLib]),
                  Acc
          end, R2, IncludeLibs),
    case R of
        {ok, Deps, Warnings} ->
            {ok, lists:reverse(Deps), lists:reverse(Warnings)}
    end.

-spec beam_file_contents_key(proplists:proplist(), [file:name()],
                             cas:cas_context()) -> binary().
beam_file_contents_key(ErlcOpts, Deps, CC) ->
    ErlcOptsBin = term_to_binary(ErlcOpts),
    FilesDigests = cas:digests_in_context(CC, Deps),
    crypto:hash(sha, [ErlcOptsBin | FilesDigests]).

-spec app_graph(#{atom() := target()}, module_index(), cas:cas_context()) -> digraph:graph().
app_graph(Targets, ModuleIndex, CC) ->
    G = digraph:new([acyclic]),
    lists:foreach(fun (App) ->
                      digraph:add_vertex(G, App)
                  end, maps:keys(Targets)),
    app_graph(maps:keys(Targets), Targets, ModuleIndex, G, CC).

app_graph([], _, _, G, _) ->
    G;
app_graph([App | Rest], Targets, ModuleIndex, G, CC) ->
    #{App := #{compile_opts := CompileOpts, outs := Outs}} = Targets,
    CopiedSrcs = lists:filter(fun is_erlang_source/1, Outs),
    app_deps(App, CopiedSrcs, CompileOpts, ModuleIndex, G, CC),
    app_graph(Rest, Targets, ModuleIndex, G, CC).

app_deps(_, [], _, _, _, _) ->
    ok;
app_deps(AppName, [SrcFile | Rest], CompileOpts, ModuleIndex, G, CC) ->
    ErlAttrs = get_analysis(SrcFile, CompileOpts, CC),
    %% ErlAttrs = cas:get_analysis_file_contents(AnalysisFile, CC),

    #{behaviour := Behaviours,
      parse_transform := Transforms} = ErlAttrs,
    lists:foreach(
      fun (Module) ->
              case ModuleIndex of
                  #{Module := OtherApp} when OtherApp =/= AppName ->
                      io:format(standard_error, "app_graph: adding edge ~p <- ~p~n", [OtherApp, AppName]),
                      digraph:add_edge(G, OtherApp, AppName);
                  _ ->
                      ok
              end
      end, Behaviours ++ Transforms),
    app_deps(AppName, Rest, CompileOpts, ModuleIndex, G, CC).

-spec src_graph(atom(), [file:name()], [compile:option()], module_index(), cas:cas_context()) -> digraph:graph().
src_graph(AppName, Srcs, CompileOpts, ModuleIndex, CC) ->
    G = digraph:new([acyclic]),
    lists:foreach(fun (Src) ->
                          digraph:add_vertex(G, Src)
                  end, Srcs),
    src_graph(AppName, Srcs, Srcs, CompileOpts, ModuleIndex, G, CC).

src_graph(_, [], _, _, _, G, _) ->
    G;
src_graph(AppName, [Src | Rest], Srcs, CompileOpts, ModuleIndex, G, CC) ->
    ErlAttrs = get_analysis(Src, CompileOpts, CC),
    #{behaviour := Behaviours,
      parse_transform := Transforms} = ErlAttrs,
    lists:foreach(
      fun (Module) ->
              %% io:format(standard_error, "BehaviorOrXform: ~p~n", [ModuleString]),
              %% io:format(standard_error, "ModuleIndex: ~p => ~p~n", [ModuleString, maps:find(ModuleString, ModuleIndex)]),

              case ModuleIndex of
                  #{Module := AppName} ->
                      {value, MS} = lists:search(
                                      fun (S) ->
                                              case list_to_atom(filename:basename(S, ".erl")) of
                                                  Module -> true;
                                                  _ -> false
                                              end
                                      end, Srcs),
                      %% io:format(standard_error, "src_graph: adding edge ~p <- ~p~n", [MS, Src]),
                      digraph:add_edge(G, MS, Src);
                  _ ->
                      ok
              end
      end, Behaviours ++ Transforms),

    src_graph(AppName, Rest, Srcs, CompileOpts, ModuleIndex, G, CC).

is_erlang_source(F) ->
    case filename:extension(F) of
        ".erl" ->
            true;
        _ ->
            false
    end.

is_erlang_header(F) ->
    case filename:extension(F) of
        ".hrl" ->
            true;
        _ ->
            false
    end.

add_compile_opts_and_dest_dir_to_targets(DestDir, Targets) ->
    maps:map(
      fun (AppName, #{erlc_opts_file := ErlcOptsFile} = Props) ->
              ErlcOpts = flags_file:read(ErlcOptsFile),
              CompileOpts0 = compile_opts:transform_erlc_opts(ErlcOpts),
              CompileOpts = [{i, "include"},
                             {i, "src"},
                             {i, "../"} | CompileOpts0],
              Props#{compile_opts => CompileOpts,
                     dest_dir => filename:join(DestDir, AppName)}
      end, Targets).

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
