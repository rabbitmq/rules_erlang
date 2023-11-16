-module(rules_erlang_compiler).

-export([main/1]).

-ifdef(TEST).
-export([
         consume_to_list/1,
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

-type config() :: #{dest_dir := string(),
                    module_index := #{string() := string()},
                    targets := #{string() := #{path := string(),
                                               erlc_opts := [string()],
                                               srcs := [string()],
                                               analysis := [string()],
                                               analysis_suffix := string(),
                                               outs := [string()]}}}.

-spec main([string()]) -> no_return().
main([ConfigJsonPath]) ->
    {ok, ConfigJson} = file:read_file(ConfigJsonPath),
    {ok, RawConfig} = thoas:decode(ConfigJson),
    Config = conform_config(RawConfig),

    #{dest_dir := DestDir,
      module_index := ModuleIndex,
      targets := Targets} = Config,

    io:format(standard_error, "ERL_LIBS: ~p~n", [os:getenv("ERL_LIBS")]),

    io:format(standard_error, "Targets: ~p~n", [Targets]),

    io:format(standard_error, "DestDir: ~p~n", [DestDir]),

    %% TreeOut = os:cmd("/usr/local/bin/tree"),
    %% io:format(standard_error, "~s~n", [TreeOut]),

    ok = clone_sources(DestDir, Targets),

    AppCompileOrder = app_compilation_order(Targets, ModuleIndex),

    io:format(standard_error, "Compilation Order: ~p~n", [AppCompileOrder]),

    lists:foreach(fun (AppName) ->
                          io:format(standard_error, "Compiling ~p~n", [AppName]),
                          #{AppName := Props} = Targets,
                          compile(AppName, Props, DestDir, ModuleIndex)
                  end, AppCompileOrder),

    %% pre-first we need to copy all the sources into the output tree
    %% first we need to determine the dependencies between apps
    %% then for each app, we need to determine it's inter-file
    %% (behaviours and such) dependencies
    %% we do the compile, then we clean up the .erl files

    ok;
main(_) ->
    exit(1).

conform_target(#{<<"path">> := Path,
                 <<"erlc_opts">> := ErlcOpts,
                 <<"srcs">> := Srcs,
                 <<"analysis">> := Analysis,
                 <<"analysis_suffix">> := Suffix,
                 <<"outs">> := Outs}) ->
    #{path => binary_to_list(Path),
      erlc_opts => lists:map(fun binary_to_list/1, ErlcOpts),
      srcs => lists:map(fun binary_to_list/1, Srcs),
      analysis => lists:map(fun binary_to_list/1, Analysis),
      analysis_suffix => binary_to_list(Suffix),
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
              RP = string:prefix(Src, AppPath ++ "/"), % might not work when cloning the "root" app
              Dest = filename:join([DestDir, AppName, RP]),
              true = lists:member(Dest, Outs),
              io:format(standard_error, "Copying ~p to ~p~n", [Src, Dest]),
              {ok, _} = file:copy(Src, Dest)
      end, Srcs).

clone_sources(DestDir, Targets) ->
    maps:foreach(
      fun (AppName, Props) ->
              ok = clone_app(DestDir, AppName, Props)
      end, Targets).

app_compilation_order(Targets, ModuleIndex) ->
    % if there is only one target, we could just return it, no need to analyze
    app_compilation_order(maps:keys(Targets), Targets, ModuleIndex, []).

app_compilation_order([], _, _, Acc) ->
    Acc;
app_compilation_order([App | Rest], Targets, ModuleIndex, Acc) ->
    % read the app's json files, merge, put it in
    % Acc after the things it depends upon
    #{App := #{analysis := AnalysisFiles}} = Targets,
    Deps = app_deps(AnalysisFiles, ModuleIndex),
    io:format(standard_error, "~p depends on ~p~n", [App, Deps]),
    % need to actually update Acc resonably given Deps...
    app_compilation_order(Rest, Targets, ModuleIndex, [App | Acc]).

app_deps(AnalysisFiles, ModuleIndex) ->
    Deps = lists:foldl(
             fun (File, Acc0) ->
                     {ok, Contents} = file:read_file(File),
                     {ok, ErlAttrs} = thoas:decode(Contents),
                     %% io:format(standard_error, "ErlAttrs: ~p~n", [ErlAttrs]),
                     #{<<"behaviour">> := Behaviours} = ErlAttrs,
                     Acc1 = lists:foldl(fun (Behaviour, Acc) ->
                                                case ModuleIndex of
                                                    #{Behaviour := Dep} ->
                                                        sets:add_element(Dep, Acc);
                                                    _ ->
                                                        Acc
                                                end
                                        end, Acc0, Behaviours),
                     % need to handle include_lib, etc.
                     Acc1
             end, sets:new(), AnalysisFiles),
    sets:to_list(Deps).

src_graph(AppName, Suffix, Analysis, Srcs, ModuleIndex) ->
    G = digraph:new([acyclic]),
    lists:foreach(fun (Src) ->
                          digraph:add_vertex(G, Src)
                  end, Srcs),
    src_graph(AppName, Suffix, Analysis, Srcs, ModuleIndex, G).

% need to read the analysis for each source, and put the .erl file in
% Acc after anything that it depends on
src_graph(_, _, [], _, _, Acc) ->
    Acc;
src_graph(AppName, Suffix, [A | Rest], Srcs, ModuleIndex, Acc0) ->
    ModuleName = filename:basename(filename:basename(A, ".json"), "." ++ Suffix),
    io:format(standard_error, "Checking deps for ~p~n", [ModuleName]),
    {value, Src} = lists:search(
                     fun (S) ->
                             filename:basename(S, ".erl") == ModuleName
                     end, Srcs),
    {ok, Contents} = file:read_file(A),
    {ok, ErlAttrs} = thoas:decode(Contents),
    #{<<"behaviour">> := Behaviours,
      <<"parse_transform">> := Transforms} = ErlAttrs,
    Acc = lists:foldl(
             fun (Behavior, Acc) ->
                     case ModuleIndex of
                         #{Behavior := AppName} ->
                             {value, BS} = lists:search(
                                             fun (S) ->
                                                     case filename:basename(S, ".erl") of
                                                         Behavior -> true;
                                                         _ -> false
                                                     end
                                             end, Srcs),
                             io:format(standard_error, "Adding edge ~p -> ~p~n", [BS, Src]),
                             digraph:add_edge(Acc, BS, Src);
                         _ ->
                             Acc
                     end
             end, Acc0, Behaviours ++ Transforms),

    src_graph(AppName, Suffix, Rest, Srcs, Acc).

is_erlang_source(F) ->
    case filename:extension(F) of
        ".erl" ->
            true;
        _ ->
            false
    end.

compile(AppName,
        #{erlc_opts := ErlcOpts,
          analysis := Analysis,
          analysis_suffix := Suffix,
          outs := Outs},
        DestDir,
        ModuleIndex) ->

    CompileOpts0 = transform_erlc_opts(ErlcOpts),
    OutDir = filename:join([DestDir, AppName, "ebin"]),
    CompileOpts = [{outdir, OutDir} | CompileOpts0],
    io:format(standard_error, "using ~p~n", [CompileOpts]),
    CopiedSrcs = lists:filter(
                   fun is_erlang_source/1,
                   Outs),

    G = src_graph(AppName, Suffix, Analysis, CopiedSrcs, ModuleIndex),
    io:format(standard_error, "~p: ~p~n", [AppName, G]),

    Srcs = consume_to_list(G),
    io:format(standard_error, "~p: ~p~n", [AppName, Srcs]),

    lists:foreach(
      fun (Src) ->
              {ok, _} = compile:file(Src, CompileOpts)
      end, Srcs).

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
                      case lists:sort([P1, P1]) of
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
              case string:split(Macro, "=") of
                  [A] ->
                      {d, list_to_atom(A)};
                  [A, V] ->
                      {d, list_to_atom(A), string_to_term(V)}
              end
      end, ErlcOpts).
