-module(compiler).

-behaviour(gen_server).

-include_lib("erl_attrs_to_json/include/erl_attrs_to_json.hrl").

-include("types.hrl").

-import(executor, [
    deps/7,
    get_analysis/3,
    beam_file_contents_key/3,
    get_beam_file_contents/2
]).

-export([
    start_link/6,
    stop/0
]).
-export([
    compile/1,
    all_compiled/1,
    any_errors/0,
    wait_for_and_compile/4
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(?MODULE, {
    app_name :: atom(),
    app_dir :: file:filename_all(),
    targets :: #{atom() := target_extended()},
    code_paths :: [string()],
    module_index :: module_index(),
    mapped_inputs :: inputs(),
    waiting :: [pid()],
    results :: #{string() := compilation_result()}
}).

-spec start_link(
    atom(),
    file:filename_all(),
    #{atom() := target_extended()},
    [string()],
    module_index(),
    inputs()
) -> gen_server:start_ret().
start_link(AppName, AppDir, Targets, CodePaths, ModuleIndex, MappedInputs) ->
    gen_server:start_link(
        {local, ?MODULE},
        ?MODULE,
        [AppName, AppDir, Targets, CodePaths, ModuleIndex, MappedInputs],
        []
    ).

stop() ->
    gen_server:stop(?MODULE).

init([AppName, AppDir, Targets, CodePaths, ModuleIndex, MappedInputs]) ->
    S = #?MODULE{
        app_name = AppName,
        app_dir = AppDir,
        targets = Targets,
        code_paths = CodePaths,
        module_index = ModuleIndex,
        mapped_inputs = MappedInputs,
        waiting = [],
        results = #{}
    },
    {ok, S}.

-spec compile(digraph:graph()) -> #{string() := compilation_result()}.
compile(SrcGraph) ->
    gen_server:call(?MODULE, {compile, SrcGraph}, 120_000).

-spec all_compiled([string()]) -> boolean().
all_compiled([]) ->
    true;
all_compiled(Deps) ->
    gen_server:call(?MODULE, {all_compiled, Deps}, 120_000).

-spec any_errors() -> boolean().
any_errors() ->
    gen_server:call(?MODULE, any_errors, 120_000).

handle_call({compile, SrcGraph}, From, #?MODULE{app_dir = AppDir} = S) ->
    Srcs = digraph:vertices(SrcGraph),
    Pids = lists:map(
        fun(Src) ->
            Deps = digraph:in_neighbours(SrcGraph, Src),
            case length(Deps) of
                0 ->
                    ok;
                _ ->
                    SrcRel = string:prefix(Src, AppDir ++ "/"),
                    DepsRel = lists:map(
                        fun(Dep) ->
                            string:prefix(Dep, AppDir ++ "/")
                        end,
                        Deps
                    ),
                    io:format(
                        standard_error,
                        "        ~p will wait for ~p~n",
                        [SrcRel, DepsRel]
                    )
            end,
            spawn_link(
                node(),
                ?MODULE,
                wait_for_and_compile,
                [From, Deps, Src, S]
            )
        end,
        Srcs
    ),
    {noreply, S#?MODULE{waiting = Pids}};
handle_call({all_compiled, Srcs}, _, #?MODULE{results = Results} = S) ->
    R = lists:all(
        fun(Src) ->
            maps:is_key(Src, Results)
        end,
        Srcs
    ),
    {reply, R, S};
handle_call(any_errors, _, #?MODULE{results = Results} = S) ->
    R = lists:any(
        fun
            ({error, _, _}) ->
                true;
            ({ok, _, _}) ->
                false
        end,
        maps:values(Results)
    ),
    {reply, R, S}.

handle_cast({finished, Sender, From, Src, R}, #?MODULE{waiting = Waiting0, results = Results0} = S) ->
    Waiting = lists:delete(Sender, Waiting0),
    lists:foreach(
        fun(W) ->
            W ! src_compiled
        end,
        Waiting
    ),
    Results = Results0#{Src => R},
    case Waiting of
        [] ->
            gen_server:reply(From, Results);
        _ ->
            ok
    end,
    {noreply, S#?MODULE{
        waiting = Waiting,
        results = Results
    }}.

-spec wait_for_and_compile(gen_server:from(), [string()], string(), #?MODULE{}) -> ok.
wait_for_and_compile(From, Deps, Src, S) ->
    case any_errors() of
        true ->
            R = {error, [], []},
            gen_server:cast(?MODULE, {finished, self(), From, Src, R});
        false ->
            case all_compiled(Deps) of
                false ->
                    receive
                        src_compiled ->
                            wait_for_and_compile(From, Deps, Src, S)
                    end;
                true ->
                    % this isn't the up to date state, the static
                    % portions of the state should be passed directly
                    % to this named function
                    #?MODULE{
                        app_name = AppName,
                        app_dir = AppDir,
                        targets = Targets,
                        code_paths = CodePaths,
                        module_index = ModuleIndex,
                        mapped_inputs = MappedInputs
                    } = S,
                    #{AppName := #{compile_opts := CompileOpts0, outs := _Outs}} = Targets,
                    CompileOpts = [
                        {outdir, "ebin"},
                        binary,
                        return,
                        no_spawn_compiler_process
                        | CompileOpts0
                    ],
                    % io:format(standard_error, "Compiling ~p with ~p~n", [AppName, CompileOpts]),

                    IncludePaths = lists:filtermap(
                        fun
                            ({i, P}) ->
                                {true, P};
                            (_) ->
                                false
                        end,
                        CompileOpts
                    ),

                    SrcRel = string:prefix(Src, AppDir ++ "/"),
                    ContentsFun = fun() ->
                        case compile:file(SrcRel, CompileOpts) of
                            {ok, Module, ModuleBin} ->
                                {ok, Module, ModuleBin, []};
                            R ->
                                R
                        end
                    end,
                    Contents =
                        case maps:size(MappedInputs) of
                            0 ->
                                % when the worker is used in one-shot
                                % fashion, MappedInputs is empty
                                ContentsFun();
                            _ ->
                                case get_analysis(Src, CompileOpts0, MappedInputs) of
                                    {ok, ErlAttrs} ->
                                        case
                                            deps(
                                                Src,
                                                ErlAttrs,
                                                IncludePaths,
                                                AppName,
                                                Targets,
                                                CodePaths,
                                                ModuleIndex
                                            )
                                        of
                                            {ok, AllDeps, []} ->
                                                Key = beam_file_contents_key(
                                                    CompileOpts0,
                                                    AllDeps,
                                                    MappedInputs
                                                ),
                                                get_beam_file_contents(Key, ContentsFun);
                                            {ok, AllDeps, DepsWarnings} ->
                                                Key = beam_file_contents_key(
                                                    CompileOpts0,
                                                    AllDeps,
                                                    MappedInputs
                                                ),
                                                case get_beam_file_contents(Key, ContentsFun) of
                                                    {ok, M, MB, CW} ->
                                                        {ok, M, MB, [{Src, DepsWarnings} | CW]};
                                                    {error, E, CW} ->
                                                        {error, E, [{Src, DepsWarnings} | CW]}
                                                end
                                        end;
                                    {error, Reason} ->
                                        Key = beam_file_contents_key(
                                            CompileOpts0,
                                            [Src],
                                            MappedInputs
                                        ),
                                        case get_beam_file_contents(Key, ContentsFun) of
                                            {ok, M, MB, CW} ->
                                                {ok, M, MB, [{Src, Reason} | CW]};
                                            {error, E, CW} ->
                                                {error, E, [{Src, Reason} | CW]}
                                        end
                                end
                        end,
                    R =
                        case Contents of
                            {ok, Module, ModuleBin, Warnings} ->
                                ModulePath = filename:join("ebin", atom_to_list(Module) ++ ".beam"),
                                ok = file:write_file(ModulePath, ModuleBin),
                                %% {module, Module} = code:load_binary(Module,
                                %%                                     filename:absname(ModulePath),
                                %%                                     ModuleBin),
                                {ok, Module, Warnings};
                            {error, Errors, Warnings} ->
                                io:format(
                                    standard_error,
                                    "Compilation error in ~p ~p~n"
                                    "  Errors: ~p~n"
                                    "  Warnings: ~p~n",
                                    [AppName, Src, Errors, Warnings]
                                ),
                                {error, Errors, Warnings}
                        end,
                    gen_server:cast(?MODULE, {finished, self(), From, Src, R})
            end
    end.
