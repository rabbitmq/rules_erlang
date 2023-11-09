-module(erl_attrs_to_json_worker).

-include_lib("erl_attrs_to_json/include/erl_attrs_to_json.hrl").

-export([main/1]).

-ifdef(TEST).
-export([conform_request/1]).
-endif.

-type input() :: #{path := string(), digest := string()}.
-type request() :: #{arguments := [string()], inputs := [input()]}.

-type response() :: #{exit_code := integer(), output := string()}.

-spec main([string()]) -> no_return().
main(["--persistent_worker"] = Args) ->
    case io:get_line("") of
        eof ->
            ok;
        Line ->
            {ok, RawRequest} = thoas:decode(Line),
            io:format(standard_error, "RawRequest: ~p~n", [RawRequest]),
            Request = conform_request(RawRequest),
            Response = execute(Request),
            %% io:format(standard_error, "Map: ~p~n", [Map]),
            Json = thoas:encode(conform_response(Response)),
            io:format("~ts", [Json]),
            % signal to caller that the json response is written
            io:format(<<0>>),
            main(Args)
    end;
main(["@" ++ FlagsFilePath]) ->
    Request = #{arguments => read_flags_file(FlagsFilePath), inputs => [], request_id => 0},
    %% io:format(standard_error, "One Shot Request: ~p~n", [Request]),
    #{exit_code := ExitCode, output := Output} = execute(Request),
    case ExitCode of
        0 ->
            io:format(standard_error, "~s~n", [Output]);
        _ ->
            io:format(standard_error, "ERROR: ~s~n", [Output])
    end,
    exit(ExitCode);
main([]) ->
    exit(1).

-spec conform_request(thoas:json_term()) -> request().
conform_request(JsonTerm) when is_map(JsonTerm) ->
    #{arguments => maps:get(<<"arguments">>, JsonTerm),
      inputs => maps:get(<<"inputs">>, JsonTerm)}.

-spec conform_response(response()) -> thoas:input_term().
conform_response(#{exit_code := ExitCode, output := Output}) ->
    #{exitCode => ExitCode,
      output => list_to_binary(Output)}.

-spec execute(request()) -> response().
execute(#{arguments := [InFile, OutFile], inputs := _Inputs}) when is_binary(InFile), is_binary(OutFile) ->
    In = binary_to_list(InFile),
    Out = binary_to_list(OutFile),
    case erl_attrs_to_json:parse(In, [], []) of
        null ->
            #{exit_code => 1,
              output => io_lib:format("Failed to parse ~p~n", [In])};
        Map ->
            case file:write_file(Out, thoas:encode(Map)) of
                ok ->
                    #{exit_code => 0,
                      output => ""};
                {error, Reason} ->
                    #{exit_code => 1,
                      output => io_lib:format("Failed to write ~p: ~p~n", [Out, Reason])}
            end
    end;
execute(_) ->
    #{exit_code => 1,
      output => "Not implemented."}.

read_flags_file(Path) ->
    {ok, D} = file:open(Path, [read]),
    try all_lines(D)
    after file:close(D)
    end.

all_lines(D) ->
    case io:get_line(D, "") of
        eof -> [];
        L -> [string:trim(L, trailing, "\n") | all_lines(D)]
    end.
