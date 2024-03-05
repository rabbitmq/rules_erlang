-module(rebar_config_to_erlc_opts).

-export([main/1]).

-ifdef(TEST).
-export([erlc_opts/1]).
-endif.

-spec main([string()]) -> no_return().
main([RebarConfig, OutFile]) ->
    ErlcOpts = erlc_opts(RebarConfig),
    Contents = string:join(ErlcOpts, "\n"),
    file:write_file(OutFile, Contents);
main([]) ->
    exit(1).

erlc_opts(RebarConfig) ->
    ErlcOpts =
        case rebar_config:parse(RebarConfig) of
            #{erl_opts := ErlOpts} ->
                lists:flatmap(
                    fun
                        (#{value := V, kind := erlc}) ->
                            [io_lib:format("+~s", [V])];
                        (_) ->
                            []
                    end,
                    ErlOpts
                );
            _ ->
                ["+debug_info"]
        end,
    lists:delete("+warnings_as_errors", lists:usort(["+deterministic" | ErlcOpts])).
