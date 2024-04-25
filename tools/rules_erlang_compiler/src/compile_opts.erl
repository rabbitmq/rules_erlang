-module(compile_opts).

-export([transform_erlc_opts/1]).

-spec string_to_term(string()) -> term().
string_to_term(S) ->
    {ok, Tokens, _} = erl_scan:string(S ++ "."),
    {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

-spec transform_erlc_opts([string()]) -> [compile:option()].
transform_erlc_opts(ErlcOpts) ->
    lists:map(
        fun
            ("-Werror") ->
                warnings_as_errors;
            ("+" ++ Term) ->
                list_to_atom(Term);
            ("-D" ++ Macro) ->
                {d, list_to_atom(Macro)};
            ("'-D" ++ Macro) ->
                M = string:strip(Macro, right, $'),
                case string:split(M, "=") of
                    [A, V] ->
                        {d, list_to_atom(A), string_to_term(V)}
                end
        end,
        ErlcOpts
    ).
