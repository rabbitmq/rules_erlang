-module(erl_attrs_to_json_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          basic,
          test_src,
          missing
         ].

basic(Config) ->
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
       erl_attrs_to_json:parse(filename:join([?config(data_dir, Config), "basic.erl"]), [], ["test"])),
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
       erl_attrs_to_json:parse(filename:join([?config(data_dir, Config), "basic.erl"]), ['TEST'], ["test"])).

test_src(Config) ->
    ?assertMatch(
       {ok, #{
              include_lib := [<<"proper/include/proper.hrl">>],
              parse_transform := [eunit_autoexport],
              call := #{
                        proper := [counterexample]
                       }
             }},
       erl_attrs_to_json:parse(filename:join([?config(data_dir, Config), "test.erl"]), ['TEST'], ["test"])).

missing(Config) ->
    ?assertMatch(
       {error, _},
       erl_attrs_to_json:parse(filename:join([?config(data_dir, Config), "missing.erl"]), ['TEST'], ["test"])).
