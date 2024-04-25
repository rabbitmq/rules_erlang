-module(flags_file).

-export([read/1]).

-spec read(file:name_all()) -> [string()].
read(Path) ->
    {ok, D} = file:open(Path, [read]),
    try
        all_lines(D)
    after
        file:close(D)
    end.

all_lines(D) ->
    case io:get_line(D, "") of
        eof -> [];
        L -> [string:trim(L, trailing, "\n") | all_lines(D)]
    end.
