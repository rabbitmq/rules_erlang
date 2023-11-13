-module(rules_erlang_compiler).

-export([main/1]).

% -ifdef(TEST).
% -export([conform_request/1]).
% -endif.

% -type input() :: #{path := string(), digest := string()}.
% -type request_args() :: #{in := string(),
%                           out := string(),
%                           macros := [macro()],
%                           includes := [include()]}.
% -type request() :: #{arguments := request_args(),
%                      inputs := [input()],
%                      request_id => integer()}.

% -type response() :: #{exit_code := integer(), output := string()}.

-spec main([string()]) -> no_return().
main(Args) ->
    io:format(standard_error, "Args: ~p~n", [Args]),
    exit(1);
% main(["--persistent_worker"] = Args) ->
%     case io:get_line("") of
%         eof ->
%             ok;
%         Line ->
%             {ok, RawRequest} = thoas:decode(Line),
%             io:format(standard_error, "RawRequest: ~p~n", [RawRequest]),
%             Request = conform_request(RawRequest),
%             io:format(standard_error, "Request: ~p~n", [Request]),
%             Response = execute(Request),
%             %% io:format(standard_error, "Map: ~p~n", [Map]),
%             Json = thoas:encode(conform_response(Response)),
%             io:format("~ts", [Json]),
%             % signal to caller that the json response is written
%             io:format(<<0>>),
%             main(Args)
%     end;
% main(["@" ++ FlagsFilePath]) ->
%     RawRequest = #{<<"arguments">> => read_flags_file(FlagsFilePath),
%                    <<"inputs">> => []},
%     Request = conform_request(RawRequest),
%     %% io:format(standard_error, "One Shot Request: ~p~n", [Request]),
%     #{exit_code := ExitCode, output := Output} = execute(Request),
%     case ExitCode of
%         0 ->
%             io:format(standard_error, "~s", [Output]),
%             ok;
%         _ ->
%             io:format(standard_error, "ERROR: ~s", [Output]),
%             exit(ExitCode)
%    end;
main([]) ->
    exit(1).
