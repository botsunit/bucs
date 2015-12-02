-module(bucuri_tests).

-include_lib("eunit/include/eunit.hrl").

bucuri_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_join())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_join() ->
  ?assertMatch(<<"http://example.com/hello/world">>,
               bucuri:join([<<"http://example.com">>, "hello", <<"world">>])),
  ?assertMatch("http://example.com/hello/world",
               bucuri:join(["http://example.com", "hello", "world"])),
  ?assertMatch("http://example.com/hello/world/",
               bucuri:join(["http://example.com/", "/hello", "/world/"])),
  ?assertMatch("http://example.com/hello/world",
               bucuri:join(["http://example.com/", "/hello/", "/world"])).

