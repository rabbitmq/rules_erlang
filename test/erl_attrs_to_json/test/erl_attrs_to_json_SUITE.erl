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
                   macros => #{'TEST' => null},
                   includes => [<<"/some/path">>]}),
    ct:pal(?LOW_IMPORTANCE, "Command1: ~p", [Command1]),
    ?assertEqual(
       #{path => "/some/path/foo.erl",
         macros => ['TEST'],
         includes => ["/some/path"]},
       erl_attrs_to_json:parse_command(binary_to_list(Command1))),

    Command2 = thoas:encode(
                 #{path => <<"/some/path/bar.erl">>,
                   macros => #{'TEST' => <<"1">>},
                   includes => [<<"/some/path">>]}),
    ct:pal(?LOW_IMPORTANCE, "Command2: ~p", [Command2]),
    ?assertEqual(
       #{path => "/some/path/bar.erl",
         macros => [{'TEST', "1"}],
         includes => ["/some/path"]},
       erl_attrs_to_json:parse_command(binary_to_list(Command2))),

    Command3 = "{\"path\":\"/private/var/folders/49/28hfxmws7651mjmlvfn1_h7h0000gp/T/gazelle_test429687135/rebar_with_ct_suites/src/osiris.erl\",\"macros\":{},\"includes\":[\"/private/var/folders/49/28hfxmws7651mjmlvfn1_h7h0000gp/T/gazelle_test429687135/rebar_with_ct_suites\",\"/private/var/folders/49/28hfxmws7651mjmlvfn1_h7h0000gp/T/gazelle_test429687135/rebar_with_ct_suites/include\",\"/private/var/folders/49/28hfxmws7651mjmlvfn1_h7h0000gp/T/gazelle_test429687135/rebar_with_ct_suites/src\"]}",
    ct:pal(?LOW_IMPORTANCE, "Command3: ~s", [Command3]),
    ?assertMatch(
      #{path := "/private/var/folders/49/28hfxmws7651mjmlvfn1_h7h0000gp/T/gazelle_test429687135/rebar_with_ct_suites/src/osiris.erl"},
      erl_attrs_to_json:parse_command(Command3)).

basic(_) ->
    ?assertEqual(
       #{
         include_lib => [],
         include => ["my_header.hrl", "basic.hrl"],
         behaviour => [],
         parse_transform => [my_pt],
         call => #{
                   filename => [split],
                   io => [format],
                   maps => [merge],
                   other_lib => [foo, bar, finalize, encode, calc],
                   some_other_lib => [bar, baz, fizz]
                  }
        },
       erl_attrs_to_json:parse(fixture_path("test/basic.erl"), [], ["test"])),
    ?assertEqual(
       #{
         include_lib => ["some_lib/include/some_header.hrl"],
         include => ["my_header.hrl", "basic.hrl"],
         behaviour => [],
         parse_transform => [my_pt],
         call => #{
                   filename => [split],
                   io => [format],
                   maps => [merge],
                   other_lib => [foo, bar, finalize, encode, calc],
                   some_other_lib => [bar, baz, fizz]
                  }
        },
       erl_attrs_to_json:parse(fixture_path("test/basic.erl"), ['TEST'], ["test"])).

test_src(_) ->
   ?assertMatch(
      #{
        include_lib := ["proper/include/proper.hrl"],
        parse_transform := [eunit_autoexport],
        call := #{
         proper := [counterexample]
        }
       },
      erl_attrs_to_json:parse(fixture_path("test/test.erl"), ['TEST'], ["test"])).

fixture_path(File) ->
    filename:join([os:getenv("TEST_SRCDIR"),
                   os:getenv("TEST_WORKSPACE"),
                   "erl_attrs_to_json",
                   File]).
