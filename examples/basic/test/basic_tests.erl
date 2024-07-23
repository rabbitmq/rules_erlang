-module(basic_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test_() ->
  [?_test(?assertEqual(true, true))].
