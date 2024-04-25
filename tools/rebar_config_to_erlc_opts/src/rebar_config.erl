-module(rebar_config).

-export([parse/1]).

mapify_dep(Name) when is_atom(Name) ->
    #{
        name => Name,
        kind => hex
    };
mapify_dep({Name, Version}) when is_list(Version) ->
    #{
        name => Name,
        kind => hex,
        version => Version
    };
mapify_dep({Name, Version, {git = Kind, Remote, Ref}}) ->
    #{
        name => Name,
        kind => Kind,
        version => Version,
        remote => Remote,
        ref => Ref
    };
mapify_dep({Name, {git = Kind, Remote, Ref}}) ->
    #{
        name => Name,
        kind => Kind,
        remote => Remote,
        ref => Ref
    };
mapify_dep({Name, {hg = Kind, Remote, Ref}}) ->
    #{
        name => Name,
        kind => Kind,
        remote => Remote,
        ref => Ref
    };
%% legacy formats
mapify_dep({Name, {git = Kind, Remote}}) ->
    #{
        name => Name,
        kind => Kind,
        remote => Remote
    };
mapify_dep({Name, Version, {git = Kind, Remote}}) ->
    #{
        name => Name,
        kind => Kind,
        version => Version,
        remote => Remote
    };
mapify_dep({Name, Version, {git = Kind, Remote, Ref}, [raw]}) ->
    #{
        name => Name,
        kind => Kind,
        version => Version,
        remote => Remote,
        ref => Ref
    };
%% other formats
mapify_dep({Name, {pkg, _}}) ->
    #{
        name => Name,
        kind => hex
    }.

conformErlOpt(Opt) when is_atom(Opt) ->
    #{
        kind => erlc,
        value => Opt
    };
conformErlOpt({i, Include}) ->
    #{
        kind => include,
        value => Include
    };
conformErlOpt({platform_define, _Platform, _Key}) ->
    #{
        kind => platform_define,
        value => ignored
    };
conformErlOpt({platform_define, _Platform, _Key, _Value}) ->
    #{
        kind => platform_define,
        value => ignored
    };
conformErlOpt({src_dirs, _SrcDirs}) ->
    #{
        kind => src_dirs,
        value => ignored
    };
conformErlOpt(Opt) ->
    #{
        kind => unknown,
        value => Opt
    }.

conformConfig(List) ->
    maps:map(
        fun
            (erl_opts, Opts) ->
                [conformErlOpt(O) || O <- Opts];
            (deps, Deps) ->
                [mapify_dep(D) || D <- Deps];
            (_, _) ->
                ignored
        end,
        proplists:to_map(List)
    ).

mapify_pkg({Name, {pkg, Pkg, Version}, _}) ->
    #{
        name => Name,
        pkg => Pkg,
        version => Version
    }.

conformLock([{V, Pkgs} | _]) when is_list(Pkgs) ->
    #{
        version => V,
        pkgs => [mapify_pkg(P) || P <- Pkgs]
    }.

parse(MetadataFile) ->
    {ok, Metadata} = file:consult(MetadataFile),
    case filename:basename(MetadataFile) of
        "rebar.config" -> conformConfig(Metadata);
        "rebar.lock" -> conformLock(Metadata)
    end.
