%%% Copyright (C) 2024 Cisco Systems, Inc.
%%%
%%% Generate HTML coverage report from coverdata files
%%%
%%% Usage:
%%%   cover_report <output_dir> <apps_dir> [coverdata_pattern]
%%%
%%% Environment variables:
%%%   COVER_REPORT_BAZEL_CACHE - Bazel cache directory (default: ~/.cache/bazel)
%%%
-module(cover_report).

-mode(compile).

-export([main/1]).

-spec main([string()]) -> no_return().
main([OutputDir, AppsDir]) ->
    BazelCache = os:getenv("COVER_REPORT_BAZEL_CACHE", 
                           filename:join([os:getenv("HOME"), ".cache", "bazel"])),
    Pattern = filename:join([BazelCache, "**", "testlogs", "**", "*.coverdata"]),
    main([OutputDir, AppsDir, Pattern]);

main([OutputDir, AppsDir, Pattern]) ->
    io:format("Generating coverage report...~n"),
    io:format("  Output: ~s~n", [OutputDir]),
    io:format("  Apps: ~s~n", [AppsDir]),
    
    %% Find coverdata files
    CoverdataFiles = find_coverdata_files(Pattern),
    io:format("Found ~p coverdata files~n", [length(CoverdataFiles)]),
    
    %% Import all coverdata
    lists:foreach(fun(F) ->
        case cover:import(F) of
            ok -> ok;
            {error, Reason} -> 
                io:format(standard_error, "Warning: Failed to import ~s: ~p~n", [F, Reason])
        end
    end, CoverdataFiles),
    
    %% Get all imported modules
    AllModules = cover:imported_modules(),
    io:format("Imported ~p modules~n", [length(AllModules)]),
    
    %% Build source file map
    SrcFiles = filelib:wildcard(filename:join([AppsDir, "*", "src", "*.erl"])),
    SrcMap = maps:from_list([{list_to_atom(filename:basename(F, ".erl")), F} || F <- SrcFiles]),
    io:format("Found ~p source files~n", [maps:size(SrcMap)]),
    
    %% Create output directory
    ok = filelib:ensure_dir(filename:join(OutputDir, "dummy")),
    
    %% Generate per-module reports
    Results = lists:filtermap(fun(M) ->
        case maps:get(M, SrcMap, undefined) of
            undefined -> false;
            SrcFile ->
                case cover:analyse(M, calls, line) of
                    {ok, Lines} when Lines =/= [] ->
                        {Covered, Total} = count_coverage(Lines),
                        case Total > 0 of
                            true ->
                                Pct = (Covered * 100) div Total,
                                OutFile = filename:join(OutputDir, atom_to_list(M) ++ ".html"),
                                generate_module_html(M, SrcFile, Lines, OutFile),
                                AppName = get_app_name(SrcFile),
                                {true, {M, Covered, Total, Pct, AppName}};
                            false -> false
                        end;
                    _ -> false
                end
        end
    end, AllModules),
    
    %% Sort by app name then module
    SortedResults = lists:sort(fun({_,_,_,_,A1}, {_,_,_,_,A2}) -> A1 =< A2 end, Results),
    
    %% Generate index
    generate_index(OutputDir, SortedResults),
    
    %% Summary
    {TotalCovered, TotalLines} = lists:foldl(
        fun({_, C, T, _, _}, {AccC, AccT}) -> {AccC + C, AccT + T} end,
        {0, 0}, SortedResults),
    TotalPct = case TotalLines of 0 -> 0; _ -> (TotalCovered * 100) div TotalLines end,
    
    io:format("~nCoverage Summary:~n"),
    io:format("  Modules with coverage: ~p~n", [length(SortedResults)]),
    io:format("  Total lines: ~p~n", [TotalLines]),
    io:format("  Covered lines: ~p~n", [TotalCovered]),
    io:format("  Overall coverage: ~p%~n", [TotalPct]),
    io:format("~nReport: file://~s/index.html~n", [OutputDir]);

main(_) ->
    io:format(standard_error, "Usage: cover_report <output_dir> <apps_dir> [coverdata_pattern]~n", []),
    halt(1).

find_coverdata_files(_Pattern) ->
    BazelCache = os:getenv("COVER_REPORT_BAZEL_CACHE",
                           filename:join([os:getenv("HOME"), ".cache", "bazel"])),
    Cmd = "find " ++ BazelCache ++ " -name '*.coverdata' -path '*testlogs*' 2>/dev/null",
    string:tokens(os:cmd(Cmd), "\n").

get_app_name(SrcFile) ->
    Parts = filename:split(SrcFile),
    find_app_name(Parts).

find_app_name(["apps", AppName | _]) -> AppName;
find_app_name([_ | Rest]) -> find_app_name(Rest);
find_app_name([]) -> "unknown".

count_coverage(Lines) ->
    lists:foldl(fun({{_, _}, Count}, {Covered, Total}) ->
        case Count of
            0 -> {Covered, Total + 1};
            _ -> {Covered + 1, Total + 1}
        end
    end, {0, 0}, Lines).

generate_module_html(Module, SrcFile, Lines, OutFile) ->
    {ok, Content} = file:read_file(SrcFile),
    SrcLines = string:tokens(binary_to_list(Content), "\n"),
    LineMap = maps:from_list([{L, C} || {{_, L}, C} <- Lines]),

    {ok, F} = file:open(OutFile, [write]),
    io:format(F, "<!DOCTYPE html><html><head>~n", []),
    io:format(F, "<title>~s</title>~n", [Module]),
    io:format(F, "<style>~n", []),
    io:format(F, "body { font-family: monospace; margin: 20px; }~n", []),
    io:format(F, ".line { white-space: pre; }~n", []),
    io:format(F, ".covered { background-color: #c8f7c5; }~n", []),
    io:format(F, ".uncovered { background-color: #f7c5c5; }~n", []),
    io:format(F, ".linenum { display: inline-block; width: 50px; color: #888; text-align: right; margin-right: 10px; }~n", []),
    io:format(F, ".count { display: inline-block; width: 60px; color: #666; text-align: right; margin-right: 10px; }~n", []),
    io:format(F, "</style></head><body>~n", []),
    io:format(F, "<h1>~s</h1>~n", [Module]),
    io:format(F, "<p><a href=\"index.html\">&larr; Back to index</a></p>~n", []),
    io:format(F, "<p>Source: ~s</p>~n", [SrcFile]),

    lists:foreach(fun({LineNum, LineContent}) ->
        EscapedContent = escape_html(LineContent),
        case maps:get(LineNum, LineMap, undefined) of
            undefined ->
                io:format(F, "<div class=\"line\"><span class=\"linenum\">~p</span><span class=\"count\"></span>~s</div>~n",
                          [LineNum, EscapedContent]);
            0 ->
                io:format(F, "<div class=\"line uncovered\"><span class=\"linenum\">~p</span><span class=\"count\">0</span>~s</div>~n",
                          [LineNum, EscapedContent]);
            Count ->
                io:format(F, "<div class=\"line covered\"><span class=\"linenum\">~p</span><span class=\"count\">~p</span>~s</div>~n",
                          [LineNum, Count, EscapedContent])
        end
    end, lists:zip(lists:seq(1, length(SrcLines)), SrcLines)),

    io:format(F, "</body></html>~n", []),
    file:close(F).

escape_html(Str) ->
    lists:flatmap(fun(C) ->
        case C of
            $< -> "&lt;";
            $> -> "&gt;";
            $& -> "&amp;";
            $" -> "&quot;";
            _ -> [C]
        end
    end, Str).

generate_index(OutputDir, Results) ->
    {ok, F} = file:open(filename:join(OutputDir, "index.html"), [write]),
    io:format(F, "<!DOCTYPE html><html><head>~n", []),
    io:format(F, "<title>Erlang Coverage Report</title>~n", []),
    io:format(F, "<style>~n", []),
    io:format(F, "body { font-family: Arial, sans-serif; margin: 20px; }~n", []),
    io:format(F, "table { border-collapse: collapse; width: 100%; }~n", []),
    io:format(F, "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }~n", []),
    io:format(F, "th { background-color: #4CAF50; color: white; }~n", []),
    io:format(F, "tr:nth-child(even) { background-color: #f2f2f2; }~n", []),
    io:format(F, ".high { color: green; } .medium { color: orange; } .low { color: red; }~n", []),
    io:format(F, ".bar { height: 20px; background-color: #ddd; }~n", []),
    io:format(F, ".bar-fill { height: 100%; }~n", []),
    io:format(F, "</style></head><body>~n", []),
    io:format(F, "<h1>Erlang Coverage Report</h1>~n", []),

    %% Overall summary
    {TotalCovered, TotalLines} = lists:foldl(
        fun({_, C, T, _, _}, {AccC, AccT}) -> {AccC + C, AccT + T} end,
        {0, 0}, Results),
    TotalPct = case TotalLines of 0 -> 0; _ -> (TotalCovered * 100) div TotalLines end,
    io:format(F, "<h2>Overall: ~p% (~p/~p lines)</h2>~n", [TotalPct, TotalCovered, TotalLines]),

    %% Group by app
    ByApp = lists:foldl(fun({M, C, T, P, App}, Acc) ->
        maps:update_with(App, fun(L) -> [{M, C, T, P} | L] end, [{M, C, T, P}], Acc)
    end, #{}, Results),

    %% App summaries table
    io:format(F, "<h2>Coverage by Application</h2>~n", []),
    io:format(F, "<table><tr><th>Application</th><th>Coverage</th><th>Lines</th><th>Bar</th></tr>~n", []),
    AppList = lists:sort(maps:to_list(ByApp)),
    lists:foreach(fun({AppName, Modules}) ->
        {AppC, AppT} = lists:foldl(fun({_, C, T, _}, {AC, AT}) -> {AC + C, AT + T} end, {0, 0}, Modules),
        AppPct = case AppT of 0 -> 0; _ -> (AppC * 100) div AppT end,
        PctClass = pct_class(AppPct),
        BarColor = bar_color(AppPct),
        io:format(F, "<tr><td>~s</td><td class=\"~s\">~p%</td><td>~p/~p</td>",
                  [AppName, PctClass, AppPct, AppC, AppT]),
        io:format(F, "<td><div class=\"bar\"><div class=\"bar-fill\" style=\"width:~p%;background-color:~s;\"></div></div></td></tr>~n",
                  [AppPct, BarColor])
    end, AppList),
    io:format(F, "</table>~n", []),

    %% Module details table
    io:format(F, "<h2>Coverage by Module</h2>~n", []),
    io:format(F, "<table><tr><th>Application</th><th>Module</th><th>Coverage</th><th>Lines</th></tr>~n", []),
    lists:foreach(fun({AppName, Modules}) ->
        SortedMods = lists:sort(fun({M1,_,_,_}, {M2,_,_,_}) -> M1 =< M2 end, Modules),
        lists:foreach(fun({M, C, T, P}) ->
            io:format(F, "<tr><td>~s</td><td><a href=\"~s.html\">~s</a></td><td class=\"~s\">~p%</td><td>~p/~p</td></tr>~n",
                      [AppName, M, M, pct_class(P), P, C, T])
        end, SortedMods)
    end, AppList),
    io:format(F, "</table></body></html>~n", []),
    file:close(F).

pct_class(Pct) when Pct >= 80 -> "high";
pct_class(Pct) when Pct >= 50 -> "medium";
pct_class(_) -> "low".

bar_color(Pct) when Pct >= 80 -> "#4CAF50";
bar_color(Pct) when Pct >= 50 -> "#ff9800";
bar_color(_) -> "#f44336".

