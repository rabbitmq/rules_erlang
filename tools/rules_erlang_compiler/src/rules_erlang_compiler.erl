-module(rules_erlang_compiler).

-export([main/1]).

% -ifdef(TEST).
% -export([conform_request/1]).
% -endif.

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

    %% pre-first we need to copy all the sources into the output tree
    %% first we need to determine the dependencies between apps
    %% then for each app, we need to determine it's inter-file
    %% (behaviours and such) dependencies
    %% we do the compile, then we clean up the .erl files


    exit(1);
main(_) ->
    exit(1).

conform_target(#{<<"path">> := Path,
                 <<"erlc_opts">> := ErlcOpts,
                 <<"srcs">> := Srcs,
                 <<"analysis">> := Analysis,
                 <<"outs">> := Outs}) ->
    #{path => binary_to_list(Path),
      erlc_opts => lists:map(fun binary_to_list/1, ErlcOpts),
      srcs => lists:map(fun binary_to_list/1, Srcs),
      analysis => lists:map(fun binary_to_list/1, Analysis),
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
    app_compilation_order(Rest, Targets, ModuleIndex, [App | Acc]).

app_deps(AnalysisFiles, ModuleIndex) ->
    Deps = lists:foldl(
             fun (File, Acc0) ->
                     {ok, Contents} = file:read_file(File),
                     {ok, ErlAttrs} = thoas:decode(Contents),
                     %% io:format(standard_error, "ErlAttrs: ~p~n", [ErlAttrs]),
                     #{<<"behaviour">> := Behaviours} = ErlAttrs,
                     lists:foldl(fun (Behaviour, Acc) ->
                                         case ModuleIndex of
                                             #{Behaviour := Dep} ->
                                                 sets:add_element(Dep, Acc);
                                             _ ->
                                                 Acc
                                         end
                                 end, Acc0, Behaviours)
             end, sets:new(), AnalysisFiles),
    sets:to_list(Deps).
