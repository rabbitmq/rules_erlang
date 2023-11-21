-module(erl_attrs_to_json_worker).

-include_lib("erl_attrs_to_json/include/erl_attrs_to_json.hrl").

-export([main/1]).

-ifdef(TEST).
-export([conform_request/1]).
-endif.

-type input() :: #{path := string(), digest := string()}.
-type request_args() :: #{in := string(),
                          out := string(),
                          macros := [macro()],
                          includes := [include()]}.
-type request() :: #{arguments := request_args(),
                     inputs := [input()],
                     request_id => integer()}.

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
            % io:format(standard_error, "Request: ~p~n", [Request]),
            Response = execute(Request),
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
    #{exit_code := ExitCode, output := Output} = execute(Request),
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
conform_request(#{<<"arguments">> := [InFile, OutFile, <<"--erlc_opts">>, ErlcOptsFile | Rest],
                  <<"inputs">> := RawInputs} = Request)
  when is_binary(InFile), is_binary(OutFile), is_binary(ErlcOptsFile) ->
    Flags = parse_flags(Rest),
    ErlcOpts = read_flags_file(ErlcOptsFile),
    Args = Flags#{in => binary_to_list(InFile),
                  out => binary_to_list(OutFile),
                  macros => macros(ErlcOpts)},

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

-spec execute(request()) -> response().
execute(#{arguments := Args}) ->
    #{in := In,
      out := Out,
      macros := Macros,
      includes := Includes} = Args,
    case erl_attrs_to_json:parse(In, Macros, Includes) of
        {ok, Map} ->
            case file:write_file(Out, thoas:encode(Map)) of
                ok ->
                    #{exit_code => 0,
                      output => ""};
                {error, Reason} ->
                    #{exit_code => 1,
                      output => io_lib:format("Failed to write ~p: ~p~n", [Out, Reason])}
            end;
        {error, Reason} ->
            #{exit_code => 1,
              output => io_lib:format("Failed to parse ~p: ~p~n", [In, Reason])}
    end.

-spec binary_string_to_term(binary()) -> term().
binary_string_to_term(B) when is_binary(B) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(B) ++ "."),
    {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

parse_flags([], Acc) ->
    Acc;
parse_flags([<<"-I">>, Path | Rest], Acc) when is_binary(Path) ->
    parse_flags(Rest,
                maps:update_with(includes,
                                 fun(L) ->
                                         [binary_to_list(Path) | L]
                                 end,
                                 Acc)).

-spec parse_flags([binary()]) -> #{includes := [include()]}.
parse_flags(Flags) ->
    parse_flags(Flags, #{includes => []}).

-spec macros([binary()]) -> [macro()].
macros(ErlcOpts) ->
    lists:filtermap(
      fun
          (<<"-D", MacroString/binary>>) ->
              Macro = case string:split(MacroString, "=") of
                          [A] -> binary_to_atom(A);
                          [A, V] -> {binary_to_atom(A), binary_string_to_term(V)}
                      end,
              {true, Macro};
          (_) ->
              false
      end, ErlcOpts).

read_flags_file(Path) ->
    {ok, D} = file:open(Path, [read]),
    try all_lines(D)
    after file:close(D)
    end.

all_lines(D) ->
    case io:get_line(D, "") of
        eof -> [];
        L -> [list_to_binary(string:trim(L, trailing, "\n")) | all_lines(D)]
    end.
