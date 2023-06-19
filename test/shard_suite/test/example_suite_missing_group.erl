-module(example_suite_missing_group).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0]).

all() ->
    [{group, a_group},
     {group, missing_group}].

groups() ->
    [{a_group, [], [one_test, two_test]}].

one_test(_Config) ->
    ?assertEqual(true, true).

two_test(_Config) ->
    ?assertEqual(true, true).
