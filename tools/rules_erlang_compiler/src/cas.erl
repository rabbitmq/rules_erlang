-module(cas).

-include("types.hrl").

-export([
         new/0,
         destroy/1,
         context/2,
         get_analysis_file_contents/2,
         analysis_file_stats/1
        ]).

-record(?MODULE, {analysis_file_contents :: ets:table()}).

-opaque cas() :: #?MODULE{}.

-opaque cas_context() :: {cas(), #{string() := string()}}.

-export_type([
              cas/0,
              cas_context/0
             ]).

-spec new() -> cas().
new() ->
    #cas{analysis_file_contents = ets:new(analysis_file_contents, [set])}.

-spec destroy(cas()) -> ok.
destroy(#cas{analysis_file_contents = Table}) ->
    ets:delete(Table),
    ok.

-spec context(cas(), [input()]) -> cas_context().
context(CAS, Inputs) ->
    InputPathToDigest = lists:foldl(
                          fun (#{path := Path, digest := Digest}, Acc) ->
                                  Acc#{Path => Digest}
                          end, #{}, Inputs),
    {CAS, InputPathToDigest}.

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

-spec analysis_file_stats(cas_context()) -> #{hits := non_neg_integer(),
                                      misses := non_neg_integer()}.
analysis_file_stats({#?MODULE{analysis_file_contents = Table}, _}) ->
    ets:foldl(
      fun ({_, C, _}, #{hits := Hits, misses := Misses} = Acc) ->
              Acc#{hits := Hits + C, misses := Misses + 1}
      end, #{hits => 0, misses => 0}, Table).
