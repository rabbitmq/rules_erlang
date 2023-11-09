-module(erl_attrs_to_json_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          conform_command
         ].

conform_command(_) ->
    Command1 = thoas:encode(
                 #{path => <<"/some/path/foo.erl">>,
                   macros => #{'TEST' => null},
                   includes => [<<"/some/path">>]}),
    ct:pal(?LOW_IMPORTANCE, "Command1: ~p", [Command1]),
    {ok, Command1Decoded} = thoas:decode(Command1),
    ?assertEqual(
       #{path => "/some/path/foo.erl",
         macros => ['TEST'],
         includes => ["/some/path"]},
       erl_attrs_to_json:conform_command(Command1Decoded)),

    Command2 = thoas:encode(
                 #{path => <<"/some/path/bar.erl">>,
                   macros => #{'TEST' => <<"1">>},
                   includes => [<<"/some/path">>]}),
    ct:pal(?LOW_IMPORTANCE, "Command2: ~p", [Command2]),
    {ok, Command2Decoded} = thoas:decode(Command2),
    ?assertEqual(
       #{path => "/some/path/bar.erl",
         macros => [{'TEST', "1"}],
         includes => ["/some/path"]},
       erl_attrs_to_json:conform_command(Command2Decoded)).
