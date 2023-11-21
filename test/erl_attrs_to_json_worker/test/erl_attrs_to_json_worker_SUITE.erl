-module(erl_attrs_to_json_worker_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          conform_request
         ].

conform_request(Config) ->
    ErlcOptsFile1 = filename:join(?config(priv_dir, Config), "erlc_opts1"),
    ok = file:write_file(ErlcOptsFile1, <<"-DTEST">>),

    Request1 = thoas:encode(
                 #{arguments => [
                                 <<"path/foo.erl">>,
                                 <<"bazel-bin/foo-arch/path/foo.json">>,
                                 <<"--erlc_opts">>, list_to_binary(ErlcOptsFile1),
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

    ErlcOptsFile2 = filename:join(?config(priv_dir, Config), "erlc_opts2"),
    ok = file:write_file(ErlcOptsFile2, <<"-DTEST=1">>),

    Request2 = thoas:encode(
                 #{arguments => [
                                 <<"path/foo.erl">>,
                                 <<"bazel-bin/foo-arch/path/foo.json">>,
                                 <<"--erlc_opts">>, list_to_binary(ErlcOptsFile2),
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
