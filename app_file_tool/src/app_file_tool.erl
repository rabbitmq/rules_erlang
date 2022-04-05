-module(app_file_tool).

-mode(compile).

-export([main/1]).

-spec main([string()]) -> no_return().
main([KeyString, ValueString, AppSrc]) ->
    % io:format(standard_error, "~p -> ~p~n", [KeyString, ValueString]),
    Key = parse_basic(KeyString),
    Value = parse_basic(ValueString),
    % io:format(standard_error, "~p -> ~p~n", [Key, Value]),
    {ok, [AppInfo]} = file:consult(AppSrc),
    {application, AppName, Props} = AppInfo,
    NewProps = lists:keystore(Key, 1, Props, {Key, Value}),
    io:format("~tp.~n", [{application, AppName, NewProps}]),
    halt();
main([EntriesString, AppSrc]) ->
    Entries = parse_basic(EntriesString),
    {ok, [AppInfo]} = file:consult(AppSrc),
    {application, AppName, Props} = AppInfo,
    NewProps = Props ++ Entries,
    io:format("~tp.~n", [{application, AppName, NewProps}]),
    halt();
main(_) ->
    halt(1).

parse_basic(String) ->
    {ok, S1, _} = erl_scan:string(String ++ "."),
    {ok, R} = erl_parse:parse_term(S1),
    R.
