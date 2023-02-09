-module(basic).

-compile({parse_transform, my_pt}).

-include("my_header.hrl").

-export([main/1]).

-ifdef(TEST).
-include_lib("some_lib/include/some_header.hrl").
-endif.

myfunc() ->
    io:format("Hello~n", []),
    {ok, some_other_lib:fizz()}.

main(_) ->
    myfunc(),
    filename:split(some_other_lib:baz()),
    _ = some_other_lib:bar(2),
    other_lib:foo(1).
