-module(rules_erlang_compiler).

-include("types.hrl").

-export([
    main/1
]).

-spec main([string()]) -> no_return().
main(["--persistent_worker"]) ->
    case os:getenv("ERLANG_HOME") of
        false ->
            io:format(standard_error, "ERROR: ERLANG_HOME env var must be set~n", []),
            exit(1);
        _ ->
            ok
    end,
    io:format(standard_error, "Worker started.~n", []),
    {ok, _} = cas:start_link(),
    worker_loop();
main(["@" ++ FlagsFilePath]) ->
    {ok, _} = cas:start_link(),
    RawRequest = #{
        <<"arguments">> => flags_file:read(FlagsFilePath),
        <<"inputs">> => []
    },
    Request = conform_request(RawRequest),
    %% io:format(standard_error, "One Shot Request: ~p~n", [Request]),
    #{
        exit_code := ExitCode,
        output := Output
    } = executor:execute(Request),
    case ExitCode of
        0 ->
            io:format(standard_error, "~s", [Output]),
            ok;
        _ ->
            io:format(standard_error, "ERROR: ~s", [Output]),
            exit(ExitCode)
    end;
main([]) ->
    exit(1).

worker_loop() ->
    case io:get_line("") of
        eof ->
            ok;
        {error, Reason} ->
            io:format(
                standard_error,
                "Error reading stdin: ~p~n",
                [Reason]
            ),
            exit(1);
        Line ->
            {ok, RawRequest} = thoas:decode(Line),
            %% io:format(standard_error, "RawRequest: ~p~n", [RawRequest]),
            Request = conform_request(RawRequest),
            #{inputs := Inputs} = Request,
            io:format(
                standard_error,
                "Request received with ~p inputs.~n",
                [maps:size(Inputs)]
            ),
            %% io:format(standard_error, "Request: ~p~n", [Request]),
            Response = executor:execute(Request),
            %% io:format(standard_error, "Map: ~p~n", [Map]),
            Json = thoas:encode(conform_response(Response)),
            io:format("~ts~n", [Json]),
            %% we should have the cas evict old stuff now
            worker_loop()
    end.

-spec conform_request(thoas:json_term()) -> request().
conform_request(
    #{
        <<"arguments">> := [ConfigJsonPath],
        <<"inputs">> := RawInputs
    } = Request
) when
    is_binary(ConfigJsonPath)
->
    Args = #{targets_file => binary_to_list(ConfigJsonPath)},

    Inputs = lists:foldl(
        fun(#{<<"path">> := Path, <<"digest">> := Digest}, Acc) ->
            Acc#{binary_to_list(Path) => base64:decode(Digest)}
        end,
        #{},
        RawInputs
    ),

    case Request of
        #{<<"request_id">> := Id} ->
            #{
                arguments => Args,
                inputs => Inputs,
                request_id => Id
            };
        _ ->
            #{
                arguments => Args,
                inputs => Inputs
            }
    end.

-spec conform_response(response()) -> thoas:input_term().
conform_response(#{exit_code := ExitCode, output := Output}) ->
    #{
        exitCode => ExitCode,
        output => list_to_binary(Output)
    }.
