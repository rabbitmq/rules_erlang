-module(basic).

-compile({parse_transform, my_pt}).

-include("my_header.hrl").

-export([main/1]).

-ifdef(TEST).
-include_lib("some_lib/include/some_header.hrl").
-endif.

myfunc(M) when is_map(M) ->
    try maps:merge(M, #{k2 => other_lib:encode(<<"string">>)})
    catch _:_ ->
        invalid
    after
        other_lib:finalize()
    end,
    io:format("Hello~n", []),
    {ok, some_other_lib:fizz()}.

main(_) ->
    myfunc(#{k => v}),
    filename:split(some_other_lib:baz()),
    _ = some_other_lib:bar(2),
    other_lib:foo(1).
