-module(cas).

-include_lib("erl_attrs_to_json/include/erl_attrs_to_json.hrl").

-include("types.hrl").

-export([
         new/0,
         destroy/1,
         context/2,
         uncontext/1,
         has_inputs/1,
         get_analysis/3,
         src_analysis_stats/1,
         get_beam_file_contents/3,
         beam_file_stats/1,
         digest_in_context/2,
         digests_in_context/2
        ]).

-record(?MODULE, {src_analysis :: ets:table(),
                  beam_file_contents :: ets:table()}).

-opaque cas() :: #?MODULE{}.

-opaque cas_context() :: {cas(), #{string() := string()}}.

-export_type([
              cas/0,
              cas_context/0
             ]).

-spec new() -> cas().
new() ->
    #cas{src_analysis = ets:new(src_analysis, [set]),
         beam_file_contents = ets:new(beam_file_file_contents, [set])}.

-spec destroy(cas()) -> ok.
destroy(#cas{src_analysis = AC,
             beam_file_contents = BFC}) ->
    ets:delete(AC),
    ets:delete(BFC),
    ok.

-spec context(cas(), #{file:name() := binary()}) -> cas_context().
context(CAS, Inputs) ->
    {CAS, Inputs}.

-spec uncontext(cas_context()) -> cas().
uncontext({CAS, _}) ->
    CAS.

-spec has_inputs(cas_context()) -> boolean().
has_inputs({_, Inputs}) ->
    maps:size(Inputs) > 0.

-spec get_analysis(binary(), fun(() -> analysis_result()), cas()) -> analysis_result().
get_analysis(Key, ContentsFun, #?MODULE{src_analysis = Table}) ->
    case ets:lookup(Table, Key) of
        [{Key, _, ErlAttrs}] ->
            ets:update_counter(Table, Key, 1),
            %% io:format(standard_error, "beam file contents cache hit ~p~n", [Key]),
            {ok, ErlAttrs};
        [] ->
            case ContentsFun() of
                {ok, ErlAttrs} ->
                    true = ets:insert_new(Table, {Key, 0, ErlAttrs}),
                    {ok, ErlAttrs};
                E ->
                    E
            end
    end.

-spec src_analysis_stats(cas()) -> #{hits := non_neg_integer(),
                                     misses := non_neg_integer()}.
src_analysis_stats(#?MODULE{src_analysis = Table}) ->
    ets:foldl(
      fun ({_, C, _}, #{hits := Hits, misses := Misses} = Acc) ->
              Acc#{hits := Hits + C, misses := Misses + 1}
      end, #{hits => 0, misses => 0}, Table).

-spec get_beam_file_contents(binary(), fun(() -> compilation_result()), cas()) -> compilation_result().
get_beam_file_contents(Key, ContentsFun, #?MODULE{beam_file_contents = Table}) ->
    case ets:lookup(Table, Key) of
        [{Key, _, Module, ModuleBin, Warnings}] ->
            ets:update_counter(Table, Key, 1),
            %% io:format(standard_error, "beam file contents cache hit ~p~n", [Key]),
            {ok, Module, ModuleBin, Warnings};
        [] ->
            case ContentsFun() of
                {ok, Module, ModuleBin, Warnings} ->
                    true = ets:insert_new(Table, {Key, 0, Module, ModuleBin, Warnings}),
                    {ok, Module, ModuleBin, Warnings};
                E ->
                    E
            end
    end.

-spec beam_file_stats(cas()) -> #{hits := non_neg_integer(),
                                  misses := non_neg_integer()}.
beam_file_stats(#?MODULE{beam_file_contents = Table}) ->
    ets:foldl(
      fun ({_, C, _, _, _}, #{hits := Hits, misses := Misses} = Acc) ->
              Acc#{hits := Hits + C, misses := Misses + 1}
      end, #{hits => 0, misses => 0}, Table).

-spec digest_in_context(cas_context(), file:name()) -> binary().
digest_in_context({_, Inputs}, File) ->
    #{File := Digest} = Inputs,
    base64:decode(Digest).

-spec digests_in_context(cas_context(), [file:name()]) -> iolist().
digests_in_context(CC, Files) ->
    lists:map(
      fun (File) ->
              digest_in_context(CC, File)
      end, Files).
