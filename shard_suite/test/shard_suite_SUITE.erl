-module(shard_suite_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          structure,
          ordered_cases,
          cases_by_group,
          shard_by_group,
          shard_by_case,
          flatten_shard,
          flatten_shard_with_nested_group
         ].

structure(_) ->
    ?assertEqual([{a_group,
                   [one_test, two_test]},
                  {another_group,
                   [two_test,
                    {nested_group, [nested_test]},
                    three_test]}],
                 shard_suite:structure(example_suite)).

ordered_cases(_) ->
    S = shard_suite:structure(example_suite),
    ?assertEqual([
                  {[a_group], one_test},
                  {[a_group], two_test},
                  {[another_group], two_test},
                  {[another_group, nested_group], nested_test},
                  {[another_group], three_test}
                 ],
                 shard_suite:ordered_cases(S)).

cases_by_group(_) ->
    S = shard_suite:structure(example_suite),
    Cases = shard_suite:ordered_cases(S),
    ?assertEqual(#{[a_group] => [one_test,two_test],
                   [another_group] => [two_test,three_test],
                   [another_group,nested_group] => [nested_test]},
                 shard_suite:cases_by_group(Cases)).

shard_by_group(_) ->
    S = shard_suite:structure(example_suite),
    Cases = shard_suite:ordered_cases(S),
    ?assertEqual({ok, [{[a_group], one_test},
                       {[a_group], two_test}]},
                 shard_suite:shard(group, Cases, 0, 3)),
    ?assertEqual({ok, [{[another_group], two_test},
                       {[another_group], three_test}]},
                 shard_suite:shard(group, Cases, 1, 3)),
    ?assertEqual({ok, [{[another_group, nested_group], nested_test}]},
                 shard_suite:shard(group, Cases, 2, 3)).

shard_by_case(_) ->
    S = shard_suite:structure(example_suite),
    Cases = shard_suite:ordered_cases(S),
    ?assertEqual({ok, [{[a_group], one_test}]},
                 shard_suite:shard('case', Cases, 0, 5)).

flatten_shard(_) ->
    S = shard_suite:structure(example_suite),
    Cases = shard_suite:ordered_cases(S),
    {ok, ShardOne} = shard_suite:shard(group, Cases, 0, 3),
    ?assertEqual(#{groups => [a_group],
                   cases => [one_test,two_test]},
                 shard_suite:flatten_shard(ShardOne)).

flatten_shard_with_nested_group(_) ->
    S = shard_suite:structure(example_suite),
    Cases = shard_suite:ordered_cases(S),
    {ok, ShardOne} = shard_suite:shard(group, Cases, 2, 3),
    %% It seems to be the case that in terms of the arguments to
    %% ct_run, we should only name the most specific group
    %% enclosing a test
    ?assertEqual(#{groups => [nested_group],
                   cases => [nested_test]},
                 shard_suite:flatten_shard(ShardOne)).
