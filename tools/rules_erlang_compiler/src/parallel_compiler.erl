-module(parallel_compiler).

-behaviour(gen_server).

-include_lib("erl_attrs_to_json/include/erl_attrs_to_json.hrl").

-include("types.hrl").

-export([
         start_link/5
        ]).
-export([
         compile_apps/1,
         all_compiled/1,
         wait_for_and_compile/4
        %  get_analysis/2,
        %  src_analysis_stats/0,
        %  get_beam_file_contents/1,
        %  put_beam_file_contents/2,
        %  beam_file_stats/0
        ]).
-export([
         init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2
        ]).

-record(?MODULE, {targets :: #{atom() := target()},
                  dest_dir :: string(),
                  code_paths :: [string()],
                  module_index :: module_index(),
                  mapped_inputs :: inputs(),
                  waiting :: [pid()],
                  results :: #{atom() := app_compilation_result()}}).

-spec start_link(#{atom() := target()}, string(), [string()], module_index(), inputs()) -> gen_server:start_ret().
start_link(Targets, DestDir, CodePaths, ModuleIndex, MappedInputs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Targets, DestDir, CodePaths, ModuleIndex, MappedInputs], []).

init([Targets, DestDir, CodePaths, ModuleIndex, MappedInputs]) ->
    S = #?MODULE{targets = Targets,
                dest_dir = DestDir,
                code_paths = CodePaths,
                module_index = ModuleIndex,
                mapped_inputs = MappedInputs,
                waiting = [],
                results = #{}},
    {ok, S}.

-spec compile_apps(digraph:graph()) -> ok.
compile_apps(AppGraph) ->
    gen_server:call(?MODULE, {compile_apps, AppGraph}, 60_000).

-spec all_compiled([atom()]) -> boolean().
all_compiled([]) ->
    true;
all_compiled(Deps) ->
    gen_server:call(?MODULE, {all_compiled, Deps}).

terminate(shutdown, _ = #?MODULE{}) ->
    ok.

handle_call({compile_apps, AppGraph}, From, S) ->
    % take a vertex and get its deps
    % call wait for and compile
    Apps = digraph:vertices(AppGraph),
    Pids = lists:map(fun (App) ->
        Deps = digraph:out_neighbours(AppGraph, App),
        io:format(standard_error,
                "        ~p will wait for ~p~n", [App, Deps]),
        spawn_link(node(),
                   ?MODULE,
                   wait_for_and_compile, [From, Deps, App, S])
    end, Apps),
    {noreply, S#?MODULE{waiting = Pids}};
handle_call({all_compiled, Deps}, _, #?MODULE{results = Results}) ->
    lists:all(fun (Dep) ->
        maps:is_key(Dep, Results)
    end, Deps).

handle_cast({finished, Sender, From, AppName, R}, #?MODULE{waiting = Waiting0, results = Results0} = S) ->
    Waiting = lists:delete(Sender, Waiting0),
    lists:foreach(fun (W) ->
        W ! app_compiled
    end, Waiting),
    Results = Results0#{AppName => R},
    case Waiting of
        [] ->
            gen_server:reply(From, Results);
        _ ->
            ok
    end,
    {stop, finished, S#?MODULE{waiting = Waiting,
                        results = Results}}.

-spec wait_for_and_compile(gen_server:from(), [atom()], atom(), #?MODULE{}) -> ok.
wait_for_and_compile(From, Deps, AppName, S) ->
    case all_compiled(Deps) of
        false ->
            receive
                app_compiled ->
                    wait_for_and_compile(From, Deps, AppName, S)
                end;
        true ->
            #?MODULE{targets = Targets,
                     dest_dir = DestDir,
                     code_paths = CodePaths,
                     module_index = ModuleIndex,
                     mapped_inputs = MappedInputs} = S,
            % we have a problem here, the function below
            % changes the cwd, so isn't safe for many processes at once
            R = executor:compile(AppName, Targets, DestDir, CodePaths, ModuleIndex, MappedInputs),
            gen_server:cast(?MODULE, {finished, self(), From, AppName, R})
    end.

% -spec compile_apps(pid(), [module()], #{atom() := target()}, string(), [string()], module_index(), inputs()) -> ok.
% compile_apps(NotifyPid, OrderedApplications, Targets, DestDir, CodePaths, ModuleIndex, MappedInputs) ->
%     R = lists:foldl(
%           fun
%               (_, {_, {error, _, _}} = E) ->
%                   E;
%               (AppName, {ModulesSoFar, {ok, Warnings}}) ->
%                   #{AppName := Props} = Targets,
%                   case compile(AppName, Targets, DestDir, CodePaths, ModuleIndex, MappedInputs) of
%                       {ok, M, []} ->
%                           case dot_app_file:render(AppName, Props, DestDir) of
%                               {ok, []} ->
%                                   {ModulesSoFar ++ M, {ok, Warnings}};
%                               {ok, W} ->
%                                   {ModulesSoFar ++ M, {ok, Warnings ++ W}}
%                           end;
%                       {ok, M, W} ->
%                           case dot_app_file:render(AppName, Props, DestDir) of
%                               {ok, []} ->
%                                   {ModulesSoFar ++ M, {ok,
%                                                        Warnings ++ [{AppName, W}]}};
%                               {ok, W} ->
%                                   {ModulesSoFar ++ M, {ok,
%                                                        Warnings ++ [{AppName, W}] ++ W}}
%                           end;
%                       {error, M, Errors, []} ->
%                           {ModulesSoFar ++ M, {error,
%                                                {AppName, Errors},
%                                                Warnings}};
%                       {error, M, Errors, W} ->
%                           {ModulesSoFar ++ M, {error,
%                                                {AppName, Errors},
%                                                Warnings ++ [{AppName, W}]}}
%                   end
%           end, {[], {ok, []}}, OrderedApplications),
%     NotifyPid ! R,
%     ok.
