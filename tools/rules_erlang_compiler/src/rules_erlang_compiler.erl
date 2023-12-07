-module(rules_erlang_compiler).

-include("types.hrl").

-export([
         main/1
        ]).

-spec main([string()]) -> no_return().
main(["--persistent_worker"]) ->
    io:format(standard_error, "Worker started.~n", []),
    InputsTable = ets:new(inputs_table, [set]),
    CAS = cas:new(),
    worker_loop(InputsTable, CAS),
    cas:destroy(CAS),
    ets:delete(InputsTable);
main(["@" ++ FlagsFilePath]) ->
    CAS = cas:new(),
    RawRequest = #{<<"arguments">> => flags_file:read(FlagsFilePath),
                   <<"inputs">> => []},
    Request = conform_request(RawRequest),
    %% io:format(standard_error, "One Shot Request: ~p~n", [Request]),
    #{exit_code := ExitCode,
      output := Output} = executor:execute(Request, CAS),
    cas:destroy(CAS),
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

worker_loop(InputsTable, CAS) ->
    case io:get_line("") of
        eof ->
            ok;
        Line ->
            {ok, RawRequest} = thoas:decode(Line),
            %% io:format(standard_error, "RawRequest: ~p~n", [RawRequest]),
            Request = conform_request(RawRequest),
            #{inputs := Inputs} = Request,
            io:format(standard_error,
                      "Request received with ~p inputs.~n",
                      [length(Inputs)]),
            update_inputs_table(InputsTable, Request),
            % io:format(standard_error, "Request: ~p~n", [Request]),
            Response = executor:execute(Request, CAS),
            %% io:format(standard_error, "Map: ~p~n", [Map]),
            Json = thoas:encode(conform_response(Response)),
            io:format("~ts~n", [Json]),
            worker_loop(InputsTable, CAS)
    end.

update_inputs_table(Table, #{inputs := Inputs}) ->
    Outgoing = ets:info(Table, size),
    Incoming = length(Inputs),
    {Same,
     Changed,
     New} = lists:foldl(
              fun (#{path := Path, digest := Digest}, {S, C, N}) ->
                      case ets:lookup(Table, Path) of
                          [{Path, Digest}] ->
                              {S + 1, C, N};
                          [{Path, _}] ->
                              ets:update_element(Table, Path, {2, Digest}),
                              {S, C + 1, N};
                          [] ->
                              true = ets:insert_new(Table, {Path, Digest}),
                              {S, C, N + 1}
                      end
              end, {0, 0, 0}, Inputs),
    Removed = ets:foldl(
                fun ({Path, _}, Acc) ->
                        case lists:search(
                               fun (#{path := P}) ->
                                       Path == P
                               end, Inputs) of
                            {value, _} ->
                                Acc;
                            _ ->
                                ets:delete(Table, Path),
                                Acc + 1
                        end
                end, 0, Table),
    io:format(standard_error,
              "Inputs updated:~n"
              "  ~p outgoing~n"
              "  ~p incoming~n"
              "  ---~n"
              "  ~p same~n"
              "  ~p updated~n"
              "  ~p new~n"
              "  ~p removed~n",
              [Outgoing, Incoming, Same, Changed, New, Removed]),
    ok.

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
