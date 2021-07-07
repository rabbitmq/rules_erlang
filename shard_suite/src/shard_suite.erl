-module(shard_suite).

-mode(compile).

-ifdef(TEST).
-compile(export_all).
-else.
-export([main/1]).
-endif.

-type sharding_method() :: 'group' | 'case'.

-type testname() :: atom().
-type groupname() :: atom().
-type suite_structure() :: [testname() | {groupname(), suite_structure()}].

-type named_case() :: {[groupname()], testname()}.

-spec main([string()]) -> no_return().
main([ShardingMethodString, SuiteModuleString, ShardIndexString, TotalShardsString]) ->
    {ShardingMethod, SuiteModule, ShardIndex, TotalShards} =
        try
            {case ShardingMethodString of
                 "-group" -> 'group';
                 "-case" -> 'case'
             end,
             list_to_atom(SuiteModuleString),
             list_to_integer(ShardIndexString),
             list_to_integer(TotalShardsString)}
        catch
            _:_ -> usage()
        end,

    ok = maybe_add_code_paths(),

    ShardingResult = try_sharding(ShardingMethod, SuiteModule, ShardIndex, TotalShards),

    Status =
        case ShardingResult of
            {ok, Shard} ->
                io:format(standard_error, "Shard: ~p~n~n", [Shard]),
                io:format(standard_error, "Shard cases: ~p~n~n", [length(Shard)]),
                #{groups := Groups,
                  cases := Cases} = flatten_shard(Shard),
                CtRunArgs = io_lib:format("-suite ~s -group ~s -case ~s",
                                          [atom_to_list(SuiteModule),
                                           lists:join(" ",
                                                      lists:map(fun atom_to_list/1, Groups)),
                                           lists:join(" ",
                                                      lists:map(fun atom_to_list/1, Cases))]),
                io:format(CtRunArgs),
                0;
            {error, Reason} ->
                io:format(standard_error, "ERROR: ~p~n", [Reason]),
                1
        end,
    halt(Status);
main(_) ->
    usage().

-spec usage() -> no_return().
usage() ->
    io:format(standard_error,
              "usage: shard_suite [-group|-uniform] suite_module shard_index total_shards~n",
              []),
    halt(1).

maybe_add_code_paths() ->
    case os:getenv("SHARD_SUITE_CODE_PATHS") of
        false ->
            ok;
        Var ->
            Paths = string:lexemes(Var, ":"),
            code:add_paths(Paths)
    end.

-spec flatten_shard([named_case()]) -> 
    #{groups => list(groupname()), cases => list(testname())}.
flatten_shard(Shard) ->
    flatten_shard(Shard, #{groups => [], cases => []}).

flatten_shard([], Acc) ->
    Acc;
flatten_shard([Case | Rest], #{groups := Groups, cases := Cases}) ->
    {Ancestors, TestCase} = Case,
    flatten_shard(Rest, #{
        groups => lists:usort(Ancestors ++ Groups),
        cases => lists:usort([TestCase | Cases])
       }).

-spec try_sharding(sharding_method(), atom(), non_neg_integer(), non_neg_integer()) ->
          {ok, [named_case()]} | {error, term()}.
try_sharding(ShardingMethod, SuiteModule, ShardIndex, TotalShards) ->
    Structure = structure(SuiteModule),

    io:format(standard_error, "Test Suite Structure: ~p~n~n", [Structure]),

    Cases = ordered_cases(Structure),

    io:format(standard_error, "Total cases: ~p~n~n", [length(Cases)]),

    case length(Cases) < TotalShards of
        true ->
            {error, not_enough_cases};
        _ ->
            shard(ShardingMethod, Cases, ShardIndex, TotalShards)
    end.

-spec shard(sharding_method(), [named_case()], non_neg_integer(), non_neg_integer()) ->
          {ok, [named_case()]} | {error, term()}.
shard('group', Cases, ShardIndex, TotalShards) ->
    CasesByGroup = cases_by_group(Cases),
    case maps:size(CasesByGroup) of
        TotalShards ->
            Ancestors = lists:nth(ShardIndex + 1,
                                  lists:sort(maps:keys(CasesByGroup))),
            Shard = lists:map(fun (TestCase) ->
                {Ancestors, TestCase}
            end, maps:get(Ancestors, CasesByGroup)),
            {ok, Shard};
        GroupCount ->
            io:format(standard_error,
                      "Suite contains ~p groups; In -group sharding mode set shard_count to match~n~n",
                      [GroupCount]),
            {error, wrong_shard_count}
    end;
shard('case', Cases, ShardIndex, TotalShards) ->
    case length(Cases) of
        TotalShards ->
            Shard = [lists:nth(ShardIndex + 1, Cases)],
            {ok, Shard};
        CaseCount ->
            io:format(standard_error,
                      "Suite contains ~p cases; In -case sharding mode set shard_count to match~n~n",
                      [CaseCount]),
            {error, wrong_shard_count}
    end.

-spec structure(atom()) -> suite_structure().
structure(SuiteModule) ->
    TestDefs = SuiteModule:all(),
    GroupDefs = case erlang:function_exported(SuiteModule, groups, 0) of
        true -> SuiteModule:groups();
        false -> []
    end,
    lists:map(fun (TestDef) ->
        structure(TestDef, GroupDefs)
    end, TestDefs).

structure({GroupName, _, SubGroupsAndTestCases}, GroupDefs) when GroupName =/= testcase ->
    Contents = lists:map(fun (S) -> structure(S, GroupDefs) end, SubGroupsAndTestCases),
    {GroupName, Contents};
structure({group, GroupName}, GroupDefs) ->
    {GroupName, _, SubGroupsAndTestCases} = lists:keyfind(GroupName, 1, GroupDefs),
    Contents = lists:map(fun (S) -> structure(S, GroupDefs) end, SubGroupsAndTestCases),
    {GroupName, Contents};
structure({testcase, TestCase, _}, _) ->
    TestCase;
structure(TestCase, _) ->
    TestCase.

-spec ordered_cases(suite_structure()) -> [named_case()].
ordered_cases(Structure) ->
    ordered_cases(Structure, []).

ordered_cases(Structure, Ancestors) ->
    lists:flatmap(fun
        ({GroupName, Contents}) ->
            ordered_cases(Contents, Ancestors ++ [GroupName]);
        (TestCase) ->
            [{Ancestors, TestCase}]
    end, Structure).

-spec cases_by_group([named_case()]) -> #{[groupname()] => [testname()]}.
cases_by_group([]) ->
    #{};
cases_by_group([Case | Rest]) ->
    {Ancestors, TestCase} = Case,
    maps:update_with(Ancestors, fun (L) ->
        [TestCase | L]
    end, [TestCase], cases_by_group(Rest)).
