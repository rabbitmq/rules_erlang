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

conform_target(#{<<"src_path">> := SrcPath,
                 <<"erlc_opts_file">> := ErlcOptsFile,
                 <<"app_src">> := AppSrc,
                 <<"srcs">> := Srcs,
                 <<"outs">> := Outs}) ->
    #{src_path => binary_to_list(SrcPath),
      erlc_opts_file => binary_to_list(ErlcOptsFile),
      app_src => case AppSrc of
                     null -> null;
                     _ -> binary_to_list(AppSrc)
                 end,
      srcs => lists:map(fun binary_to_list/1, Srcs),
      outs => lists:map(fun binary_to_list/1, Outs)}.

conform_targets(Targets) ->
    maps:fold(
      fun (K, V, Acc) ->
              Acc#{binary_to_atom(K) => conform_target(V)}
      end, #{}, Targets).

conform_index(ModuleIndex) ->
    maps:fold(
      fun (K, V, Acc) ->
              Acc#{binary_to_atom(K) => binary_to_atom(V)}
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
