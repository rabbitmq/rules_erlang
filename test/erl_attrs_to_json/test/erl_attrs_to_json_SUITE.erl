-module(erl_attrs_to_json_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          basic
         ].

basic(_) ->
    ?assertEqual(
       #{
         include_lib => [],
         include => ["my_header.hrl"],
         behaviour => [],
         call => #{}
        },
       erl_attrs_to_json:parse(fixture_path("test/basic.erl"), false)),
    ?assertEqual(
       #{
         include_lib => ["some_lib/include/some_header.hrl"],
         include => ["my_header.hrl"],
         behaviour => [],
         call => #{}
        },
       erl_attrs_to_json:parse(fixture_path("test/basic.erl"), true)).

fixture_path(File) ->
    filename:join([os:getenv("TEST_SRCDIR"),
                   os:getenv("TEST_WORKSPACE"),
                   "erl_attrs_to_json",
                   File]).
