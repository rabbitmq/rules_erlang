-module(dot_app_file_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
          {group, render_dot_app_file}
         ].

groups() -> [
             {render_dot_app_file, [], [injects_modules,
                                        injects_applications_if_missing]}
            ].

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

    dot_app_file:render("basic", Target, DestDir),

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

    dot_app_file:render("basic2", Target, DestDir),

    {ok, [{application, basic2, Props}]} = file:consult(Output),

    ?assertEqual({modules, [basic,
                            basic_acceptor,
                            basic_acceptors_sup]},
                 lists:keyfind(modules, 1, Props)),

    ?assertEqual({applications, [kernel,
                                 stdlib,
                                 other]},
                 lists:keyfind(applications, 1, Props)).
