-module(erl_attrs_to_json_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          parse_command,
          basic,
          test_src
         ].

parse_command(_) ->
    Command1 = thoas:encode(
                 #{path => <<"/some/path/foo.erl">>,
                   macros => #{'TEST' => null}}),
    ct:pal(?LOW_IMPORTANCE, "Command1: ~p", [Command1]),
    ?assertEqual(
       #{path => "/some/path/foo.erl",
         macros => ['TEST']},
       erl_attrs_to_json:parse_command(binary_to_list(Command1))),
    Command2 = thoas:encode(
                 #{path => <<"/some/path/bar.erl">>,
                   macros => #{'TEST' => <<"1">>}}),
    ct:pal(?LOW_IMPORTANCE, "Command2: ~p", [Command2]),
    ?assertEqual(
       #{path => "/some/path/bar.erl",
         macros => [{'TEST', "1"}]},
       erl_attrs_to_json:parse_command(binary_to_list(Command2))).

basic(_) ->
    ?assertEqual(
       #{
         include_lib => [],
         include => ["my_header.hrl"],
         behaviour => [],
         parse_transform => [my_pt],
         call => #{
                   io => [format],
                   other_lib => [foo],
                   some_other_lib => [bar]
                  }
        },
       erl_attrs_to_json:parse(fixture_path("test/basic.erl"), [])),
    ?assertEqual(
       #{
         include_lib => ["some_lib/include/some_header.hrl"],
         include => ["my_header.hrl"],
         behaviour => [],
         parse_transform => [my_pt],
         call => #{
                   io => [format],
                   other_lib => [foo],
                   some_other_lib => [bar]
                  }
        },
       erl_attrs_to_json:parse(fixture_path("test/basic.erl"), ['TEST'])).

test_src(_) ->
   ?assertMatch(
      #{
        include_lib := ["proper/include/proper.hrl"],
        parse_transform := [eunit_autoexport],
        call := #{
         proper := [counterexample]
        }
       },
      erl_attrs_to_json:parse(fixture_path("test/test.erl"), ['TEST'])).

fixture_path(File) ->
    filename:join([os:getenv("TEST_SRCDIR"),
                   os:getenv("TEST_WORKSPACE"),
                   "erl_attrs_to_json",
                   File]).
