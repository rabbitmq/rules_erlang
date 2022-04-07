-module(app_file_tool).

-mode(compile).

-export([main/1]).

-define(SPECIAL_MERGE_KEY, app_file_tool_special_merge_key).

-spec main([string()]) -> no_return().
main([AppSrc]) ->
    ok = io:setopts([binary]),
    Overrides = conform(decode_json_from(standard_io)),
    {ok, [AppInfo]} = file:consult(AppSrc),
    {application, AppName, Props} = AppInfo,
    NewProps = lists:foldl(fun (Override, Acc) ->
        lists:keystore(element(1, Override), 1, Acc, Override)
    end, Props, Overrides),
    io:format("~tp.~n", [{application, AppName, NewProps}]),
    halt();
main(_) ->
    halt(1).

conform(Overrides) ->
    lists:reverse(maps:fold(fun
        (?SPECIAL_MERGE_KEY, Value, Acc) ->
            Terms = parse_terms(binary_to_list(Value)),
            lists:reverse(Terms) ++ Acc;
        (Key, Value, Acc) when is_binary(Value) ->
           [{binary_to_atom(Key), binary_to_list(Value)} | Acc];
        (Key, Value, Acc) when is_list(Value) ->
            [{binary_to_atom(Key), conform_list(Value)} | Acc];
         (Key, Value, Acc) ->
            [{binary_to_atom(Key), Value} | Acc]
    end, [], Overrides)).

conform_list(Values) when is_list(Values) ->
    lists:map(fun
        (V) when is_binary(V) -> binary_to_list(V);
        (V) -> V
    end, Values).

decode_json_from(IoDevice) ->
    decode_rest(IoDevice, {incomplete, fun (Data) ->
        jsx:decode(Data, [stream])
    end}).

decode_rest(IoDevice, {incomplete, Decoder}) ->
    case file:read_line(IoDevice) of
        {ok, Data} ->
            decode_rest(IoDevice, Decoder(Data));
        eof ->
            Decoder(end_stream)
    end;
decode_rest(_, Value) ->
    Value.

parse_terms(String) ->
    {ok, Tokens, _} = erl_scan:string("[" + String ++ "]."),
    {ok, Terms} = erl_parse:parse_term(Tokens),
    Terms.
