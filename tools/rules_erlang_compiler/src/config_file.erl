-module(config_file).

-include("types.hrl").

-export([read/1]).

-spec read(string()) -> {ok, config()} | {error, term()}.
read(ConfigJsonPath) ->
    case file:read_file(ConfigJsonPath) of
        {ok, ConfigJson} ->
            case thoas:decode(ConfigJson) of
                {ok, RawConfig} ->
                    case catch conform_config(RawConfig) of
                        bad_match ->
                            {error, "config did not match expected shape"};
                        Config ->
                            {ok, Config}
                    end;
                E ->
                    E
            end;
        E ->
            E
    end.

conform_target(#{<<"path">> := Path,
                 <<"erlc_opts_file">> := ErlcOptsFile,
                 <<"app_src">> := AppSrc,
                 <<"srcs">> := Srcs,
                 <<"analysis">> := Analysis,
                 <<"analysis_id">> := AnalysisId,
                 <<"outs">> := Outs}) ->
    #{path => binary_to_list(Path),
      erlc_opts_file => binary_to_list(ErlcOptsFile),
      app_src => case AppSrc of
                     null -> null;
                     _ -> binary_to_list(AppSrc)
                 end,
      srcs => lists:map(fun binary_to_list/1, Srcs),
      analysis => lists:map(fun binary_to_list/1, Analysis),
      analysis_id => binary_to_list(AnalysisId),
      outs => lists:map(fun binary_to_list/1, Outs)}.

conform_targets(Targets) ->
    maps:fold(
      fun (K, V, Acc) ->
              Acc#{binary_to_list(K) => conform_target(V)}
      end, #{}, Targets).

conform_index(ModuleIndex) ->
    maps:fold(
      fun (K, V, Acc) ->
              % maybe these should be atoms?
              Acc#{binary_to_list(K) => binary_to_list(V)}
      end, #{}, ModuleIndex).

conform_code_paths(Dirs) ->
    lists:map(fun binary_to_list/1, Dirs).

-spec conform_config(thoas:json_term()) -> config().
conform_config(#{<<"module_index">> := MI,
                 <<"code_paths">> := CP,
                 <<"targets">> := T}) ->
    #{module_index => conform_index(MI),
      code_paths => conform_code_paths(CP),
      targets => conform_targets(T)}.
