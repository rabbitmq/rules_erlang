-module(erl_attrs_to_json_worker_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          conform_request
         ].

conform_request(_) ->
    Request1 = thoas:encode(
                 #{arguments => [
                                 <<"path/foo.erl">>,
                                 <<"bazel-bin/foo-arch/path/foo.json">>,
                                 <<"-DTEST">>,
                                 <<"-I">>, <<"/some/path">>
                                ],
                   inputs => []}),
    ct:pal(?LOW_IMPORTANCE, "Request1: ~p", [Request1]),
    {ok, Request1Decoded} = thoas:decode(Request1),
    ?assertEqual(
       #{arguments => #{in => "path/foo.erl",
                        out => "bazel-bin/foo-arch/path/foo.json",
                        macros => ['TEST'],
                        includes => ["/some/path"]},
         inputs => []},
       erl_attrs_to_json_worker:conform_request(Request1Decoded)),

    Request2 = thoas:encode(
                 #{arguments => [
                                 <<"path/foo.erl">>,
                                 <<"bazel-bin/foo-arch/path/foo.json">>,
                                 <<"-DTEST=1">>,
                                 <<"-I">>, <<"/some/path">>
                                ],
                   inputs => []}),
    ct:pal(?LOW_IMPORTANCE, "Request2: ~p", [Request2]),
    {ok, Request2Decoded} = thoas:decode(Request2),
    ?assertEqual(
       #{arguments => #{in => "path/foo.erl",
                        out => "bazel-bin/foo-arch/path/foo.json",
                        macros => [{'TEST', 1}],
                        includes => ["/some/path"]},
         inputs => []},
       erl_attrs_to_json_worker:conform_request(Request2Decoded)).
