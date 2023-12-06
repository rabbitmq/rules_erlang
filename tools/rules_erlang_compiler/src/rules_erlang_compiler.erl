-module(rules_erlang_compiler).

-include("types.hrl").

-export([
         main/1,
         read_flags_file/1
        ]).

-spec main([string()]) -> no_return().
main(["--persistent_worker"] = Args) ->
    case io:get_line("") of
        eof ->
            ok;
        Line ->
            {ok, RawRequest} = thoas:decode(Line),
            io:format(standard_error, "RawRequest: ~p~n", [RawRequest]),
            Request = conform_request(RawRequest),
            % io:format(standard_error, "Request: ~p~n", [Request]),
            Response = executor:execute(Request),
            %% io:format(standard_error, "Map: ~p~n", [Map]),
            Json = thoas:encode(conform_response(Response)),
            io:format("~ts~n", [Json]),
            main(Args)
    end;
main(["@" ++ FlagsFilePath]) ->
    RawRequest = #{<<"arguments">> => read_flags_file(FlagsFilePath),
                   <<"inputs">> => []},
    Request = conform_request(RawRequest),
    %% io:format(standard_error, "One Shot Request: ~p~n", [Request]),
    #{exit_code := ExitCode, output := Output} = executor:execute(Request),
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

-spec conform_request(thoas:json_term()) -> request().
conform_request(#{<<"arguments">> := [ConfigJsonPath],
                  <<"inputs">> := RawInputs} = Request)
  when is_binary(ConfigJsonPath) ->
    Args = #{targets_file => binary_to_list(ConfigJsonPath)},

    Inputs = lists:map(fun(#{<<"path">> := Path,
                             <<"digest">> := Digest}) ->
                               #{path => binary_to_list(Path),
                                 digest => binary_to_list(Digest)}
                       end, RawInputs),

    case Request of
        #{<<"request_id">> := Id} ->
            #{arguments => Args,
              inputs => Inputs,
              request_id => Id};
        _ ->
            #{arguments => Args,
              inputs => Inputs}
    end.

-spec conform_response(response()) -> thoas:input_term().
conform_response(#{exit_code := ExitCode, output := Output}) ->
    #{exitCode => ExitCode,
      output => list_to_binary(Output)}.

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
