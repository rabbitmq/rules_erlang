-module(rebar_config_to_erlc_opts).

-export([
    main/1
]).

-spec main([string()]) -> no_return().
main([RebarConfig, OutFile]) ->
    ErlcOpts0 =
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
    ErlcOpts = lists:delete("+warnings_as_errors", lists:usort(["+deterministic" | ErlcOpts0])),
    Contents = string:join(ErlcOpts, "\n"),
    file:write_file(OutFile, Contents);
main([]) ->
    exit(1).
