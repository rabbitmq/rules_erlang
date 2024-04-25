-module(rebar_config_to_erlc_opts_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() ->
    [
        basic
    ].

basic(Config) ->
    DataDir = ?config(data_dir, Config),
    RebarConfig = filename:join(DataDir, "rebar.config"),

    ?assertEqual(
        ["+deterministic", "+no_debug_info"],
        rebar_config_to_erlc_opts:erlc_opts(RebarConfig)
    ).
