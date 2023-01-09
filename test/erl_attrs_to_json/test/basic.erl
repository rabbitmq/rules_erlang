-module(basic).

-export([main/1]).

-include("my_header.hrl").

-ifdef(TEST).
-include_lib("some_lib/include/some_header.hrl").
-endif.

main(_) ->
    ok.
