-module(rules_erlang_compiler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          consume_to_list,
          transform_erlc_opts,
          {group, render_dot_app_file}
         ].

groups() -> [
             {render_dot_app_file, [], [injects_modules,
                                        injects_applications_if_missing]}
            ].

consume_to_list(_) ->
    G1 = digraph:new([acyclic]),

    Root = digraph:add_vertex(G1, "root"),
    Middle = digraph:add_vertex(G1, "middle"),
    Leaf1 = digraph:add_vertex(G1, "leaf1"),
    Leaf2 = digraph:add_vertex(G1, "leaf2"),
    Loner = digraph:add_vertex(G1, "loner"),

    digraph:add_edge(G1, Root, Middle),
    digraph:add_edge(G1, Middle, Leaf1),
    digraph:add_edge(G1, Middle, Leaf2),

    ?assertEqual([Root, Middle, Leaf1, Leaf2, Loner],
                 rules_erlang_compiler:consume_to_list(G1)).

transform_erlc_opts(_) ->
    ?assertEqual([warnings_as_errors,deterministic,debug_info,
                  warn_export_vars,warn_shadow_vars,
                  warn_obsolete_guard,
                  {d, namespaced_dicts}],
                 rules_erlang_compiler:transform_erlc_opts([
                                                            "-Werror",
                                                            "+deterministic",
                                                            "+debug_info",
                                                            "+warn_export_vars",
                                                            "+warn_shadow_vars",
                                                            "+warn_obsolete_guard",
                                                            "-Dnamespaced_dicts"
                                                           ])),
    ?assertEqual([warnings_as_errors,deterministic,
                  {d, namespaced_dicts},
                  {d, 'TEST', 1}],
                 rules_erlang_compiler:transform_erlc_opts([
                                                            "-Werror",
                                                            "+deterministic",
                                                            "-Dnamespaced_dicts",
                                                            "'-DTEST=1'"
                                                           ])).

injects_modules(Config) ->
    DataDir = ?config(data_dir, Config),
    DestDir = ?config(priv_dir, Config),

    Deps = sets:new(),
    sets:add_element("other", Deps),

    Target = #{app_src => filename:join(DataDir, "basic.app.src"),
               deps => Deps,
               outs => ["bazel-out/darwin-fastbuild/bin/deps_dir/basic/ebin/basic.beam",
                        "bazel-out/darwin-fastbuild/bin/deps_dir/basic/src/basic.erl",
                        "bazel-out/darwin-fastbuild/bin/deps_dir/basic/ebin/basic_acceptor.beam",
                        "bazel-out/darwin-fastbuild/bin/deps_dir/basic/src/basic_acceptor.erl",
                        "bazel-out/darwin-fastbuild/bin/deps_dir/basic/ebin/basic_acceptors_sup.beam",
                        "bazel-out/darwin-fastbuild/bin/deps_dir/basic/src/basic_acceptors_sup.erl"]},

    Output = filename:join([DestDir, "basic", "ebin", "basic.app"]),
    %% bazel normally creates the dest dir for us
    ok = filelib:ensure_dir(Output),

    rules_erlang_compiler:render_dot_app_file("basic", Target, DestDir),

    {ok, [{application, basic, Props}]} = file:consult(Output),

    ?assertEqual({modules, [basic,
                            basic_acceptor,
                            basic_acceptors_sup]},
                 lists:keyfind(modules, 1, Props)),

    % applications are not overwritten despite detected deps
    ?assertEqual({applications, [kernel,
                                 stdlib]},
                 lists:keyfind(applications, 1, Props)).

injects_applications_if_missing(Config) ->
    DataDir = ?config(data_dir, Config),
    DestDir = ?config(priv_dir, Config),

    Deps = sets:add_element("other", sets:new()),

    Target = #{app_src => filename:join(DataDir, "basic2.app.src"),
               deps => Deps,
               outs => ["bazel-out/darwin-fastbuild/bin/deps_dir/basic2/ebin/basic.beam",
                        "bazel-out/darwin-fastbuild/bin/deps_dir/basic2/src/basic.erl",
                        "bazel-out/darwin-fastbuild/bin/deps_dir/basic2/ebin/basic_acceptor.beam",
                        "bazel-out/darwin-fastbuild/bin/deps_dir/basic2/src/basic_acceptor.erl",
                        "bazel-out/darwin-fastbuild/bin/deps_dir/basic2/ebin/basic_acceptors_sup.beam",
                        "bazel-out/darwin-fastbuild/bin/deps_dir/basic2/src/basic_acceptors_sup.erl"]},

    Output = filename:join([DestDir, "basic2", "ebin", "basic2.app"]),
    %% bazel normally creates the dest dir for us
    ok = filelib:ensure_dir(Output),

    rules_erlang_compiler:render_dot_app_file("basic2", Target, DestDir),

    {ok, [{application, basic2, Props}]} = file:consult(Output),

    ?assertEqual({modules, [basic,
                            basic_acceptor,
                            basic_acceptors_sup]},
                 lists:keyfind(modules, 1, Props)),

    ?assertEqual({applications, [kernel,
                                 stdlib,
                                 other]},
                 lists:keyfind(applications, 1, Props)).
