-module(cas).

-behaviour(gen_server).

-include_lib("erl_attrs_to_json/include/erl_attrs_to_json.hrl").

-include("types.hrl").

-export([
         start_link/0
        ]).
-export([
         get_analysis/2,
         src_analysis_stats/0,
         get_beam_file_contents/2,
         beam_file_stats/0
        ]).
-export([
         init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2
        ]).

-record(?MODULE, {src_analysis :: ets:table(),
                  beam_file_contents :: ets:table()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    S = #?MODULE{src_analysis = ets:new(src_analysis, [set]),
                 beam_file_contents = ets:new(beam_file_file_contents, [set])},
    {ok, S}.

terminate(shutdown, #?MODULE{src_analysis = AC,
                             beam_file_contents = BFC}) ->
    ets:delete(AC),
    ets:delete(BFC),
    ok.

-spec get_analysis(binary(), fun(() -> analysis_result())) -> analysis_result().
get_analysis(Key, ContentsFun) ->
    gen_server:call(?MODULE, {get_analysis, Key, ContentsFun}).

-spec src_analysis_stats() -> #{hits := non_neg_integer(),
                                     misses := non_neg_integer()}.
src_analysis_stats() ->
    gen_server:call(?MODULE, analysis_stats).

-spec get_beam_file_contents(binary(), fun(() -> compilation_result())) -> compilation_result().
get_beam_file_contents(Key, ContentsFun) ->
    gen_server:call(?MODULE, {get_beam, Key, ContentsFun}, 30_000).

-spec beam_file_stats() -> #{hits := non_neg_integer(),
                             misses := non_neg_integer()}.
beam_file_stats() ->
    gen_server:call(?MODULE, beam_stats).

handle_call({get_analysis, Key, ContentsFun}, _, #?MODULE{src_analysis = Table} = S) ->
    R = case ets:lookup(Table, Key) of
            [{Key, _, ErlAttrs}] ->
                ets:update_counter(Table, Key, 1),
                {ok, ErlAttrs};
            [] ->
                case ContentsFun() of
                    {ok, ErlAttrs} ->
                        true = ets:insert_new(Table, {Key, 0, ErlAttrs}),
                        {ok, ErlAttrs};
                    E ->
                        E
                end
        end,
    {reply, R, S};
handle_call(analysis_stats, _, #?MODULE{src_analysis = Table} = S) ->
    R = ets:foldl(
          fun ({_, C, _}, #{hits := Hits, misses := Misses} = Acc) ->
                  Acc#{hits := Hits + C, misses := Misses + 1}
          end, #{hits => 0, misses => 0}, Table),
    {reply, R, S};
handle_call({get_beam, Key, ContentsFun}, _, #?MODULE{beam_file_contents = Table} = S) ->
    R = case ets:lookup(Table, Key) of
            [{Key, _, Module, ModuleBin, Warnings}] ->
                ets:update_counter(Table, Key, 1),
                {ok, Module, ModuleBin, Warnings};
            [] ->
                case ContentsFun() of
                    {ok, Module, ModuleBin, Warnings} ->
                        true = ets:insert_new(Table, {Key, 0, Module, ModuleBin, Warnings}),
                        {ok, Module, ModuleBin, Warnings};
                    E ->
                        E
                end
        end,
    {reply, R, S};
handle_call(beam_stats, _, #?MODULE{beam_file_contents = Table} = S) ->
    R = ets:foldl(
          fun ({_, C, _, _, _}, #{hits := Hits, misses := Misses} = Acc) ->
                  Acc#{hits := Hits + C, misses := Misses + 1}
          end, #{hits => 0, misses => 0}, Table),
    {reply, R, S}.

handle_cast(_, S) ->
    {noreply, S}.
