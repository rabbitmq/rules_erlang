-module(erl_attrs_to_json_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          basic,
          test_src,
          missing
         ].

basic(_) ->
    ?assertEqual(
       {ok, #{
              include_lib => [],
              include => [<<"my_header.hrl">>, <<"basic.hrl">>],
              behaviour => [],
              parse_transform => [my_pt],
              call => #{
                        filename => [split],
                        io => [format],
                        maps => [merge],
                        other_lib => [foo, bar, finalize, encode, calc],
                        some_other_lib => [bar, baz, fizz]
                       }
             }},
       erl_attrs_to_json:parse(fixture_path("test/basic.erl"), [], ["test"])),
    ?assertEqual(
       {ok, #{
              include_lib => [<<"some_lib/include/some_header.hrl">>],
              include => [<<"my_header.hrl">>, <<"basic.hrl">>],
              behaviour => [],
              parse_transform => [my_pt],
              call => #{
                        filename => [split],
                        io => [format],
                        maps => [merge],
                        other_lib => [foo, bar, finalize, encode, calc],
                        some_other_lib => [bar, baz, fizz]
                       }
             }},
       erl_attrs_to_json:parse(fixture_path("test/basic.erl"), ['TEST'], ["test"])).

test_src(_) ->
    ?assertMatch(
       {ok, #{
              include_lib := [<<"proper/include/proper.hrl">>],
              parse_transform := [eunit_autoexport],
              call := #{
                        proper := [counterexample]
                       }
             }},
       erl_attrs_to_json:parse(fixture_path("test/test.erl"), ['TEST'], ["test"])).

missing(_) ->
    ?assertMatch(
       {error, _},
       erl_attrs_to_json:parse(fixture_path("test/missing.erl"), ['TEST'], ["test"])).

fixture_path(File) ->
    filename:join([os:getenv("TEST_SRCDIR"),
                   os:getenv("TEST_WORKSPACE"),
                   "erl_attrs_to_json",
                   File]).
