-module(rules_erlang_compiler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          basic
         ].

basic(_) ->
       JSON = #{<<"bin_dir">> => <<"bazel-out/darwin-fastbuild/bin">>,
              <<"label_name">> => <<"test_deps_dir">>,
              <<"targets">> =>
              #{<<"eunit_formatters">> =>
                     #{<<"analysis">> =>
                            [<<"bazel-out/darwin-fastbuild/bin/external/rules_erlang~override~erlang_package~eunit_formatters/src/binomial_heap.normal.json">>,
                            <<"bazel-out/darwin-fastbuild/bin/external/rules_erlang~override~erlang_package~eunit_formatters/src/eunit_progress.normal.json">>],
                     <<"erlc_opts">> =>
                            [<<"-Werror">>,<<"+deterministic">>,
                            <<"+debug_info">>,<<"+warn_export_vars">>,
                            <<"+warn_shadow_vars">>,
                            <<"+warn_obsolete_guard">>,
                            <<"-Dnamespaced_dicts">>],
                     <<"outs">> =>
                            [<<"bazel-out/darwin-fastbuild/bin/test_deps_dir/eunit_formatters/ebin/binomial_heap.beam">>,
                            <<"bazel-out/darwin-fastbuild/bin/test_deps_dir/eunit_formatters/ebin/eunit_progress.beam">>],
                     <<"srcs">> =>
                            [<<"external/rules_erlang~override~erlang_package~eunit_formatters/src/binomial_heap.erl">>,
                            <<"external/rules_erlang~override~erlang_package~eunit_formatters/src/eunit_progress.erl">>]}}},

    ok.

