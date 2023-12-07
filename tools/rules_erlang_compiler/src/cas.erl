-module(cas).

-include("types.hrl").

-export([
         new/0,
         destroy/1,
         context/2,
         uncontext/1,
         has_inputs/1,
         get_analysis_file_contents/2,
         analysis_file_stats/1,
         get_beam_file_contents/3,
         beam_file_stats/1,
         digest_in_context/2
        ]).

-record(?MODULE, {analysis_file_contents :: ets:table(),
                  beam_file_contents :: ets:table()}).

-opaque cas() :: #?MODULE{}.

-opaque cas_context() :: {cas(), #{string() := string()}}.

-export_type([
              cas/0,
              cas_context/0
             ]).

-spec new() -> cas().
new() ->
    #cas{analysis_file_contents = ets:new(analysis_file_contents, [set]),
         beam_file_contents = ets:new(beam_file_file_contents, [set])}.

-spec destroy(cas()) -> ok.
destroy(#cas{analysis_file_contents = AFC,
             beam_file_contents = BFC}) ->
    ets:delete(AFC),
    ets:delete(BFC),
    ok.

-spec context(cas(), [input()]) -> cas_context().
context(CAS, Inputs) ->
    InputPathToDigest = lists:foldl(
                          fun (#{path := Path, digest := Digest}, Acc) ->
                                  Acc#{Path => Digest}
                          end, #{}, Inputs),
    {CAS, InputPathToDigest}.

-spec uncontext(cas_context()) -> cas().
uncontext({CAS, _}) ->
    CAS.

-spec has_inputs(cas_context()) -> boolean().
has_inputs({_, Inputs}) ->
    maps:size(Inputs) > 0.

-spec get_analysis_file_contents(file:name(), cas_context()) -> thoas:json_term().
get_analysis_file_contents(F,
                           {#?MODULE{analysis_file_contents = Table}, Inputs}) ->
    case maps:size(Inputs) of
        0 ->
            {ok, Contents} = file:read_file(F),
            {ok, ErlAttrs} = thoas:decode(Contents),
            ErlAttrs;
        _ ->
            case Inputs of
                #{F := Digest} ->
                    case ets:lookup(Table, Digest) of
                        [{Digest, _, ErlAttrs}] ->
                            ets:update_counter(Table, Digest, 1),
                            ErlAttrs;
                        [] ->
                            {ok, Contents} = file:read_file(F),
                            {ok, ErlAttrs} = thoas:decode(Contents),
                            true = ets:insert_new(Table, {Digest, 0, ErlAttrs}),
                            ErlAttrs
                    end
            end
    end.

-spec analysis_file_stats(cas()) -> #{hits := non_neg_integer(),
                                      misses := non_neg_integer()}.
analysis_file_stats(#?MODULE{analysis_file_contents = Table}) ->
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

-spec digest_in_context(cas_context(), [string()]) -> binary().
digest_in_context({_, Inputs}, Files) ->
    Digests = lists:map(
                fun (File) ->
                        base64:decode(maps:get(File, Inputs))
                end, Files),
    erlang:iolist_to_binary(Digests).
