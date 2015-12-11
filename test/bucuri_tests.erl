-module(bucuri_tests).

-include_lib("eunit/include/eunit.hrl").

bucuri_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_join())
    , ?_test(t_join_with_uri())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_join() ->
  ?assertMatch(<<"/path/to/my/ressource">>,
               bucuri:join([<<"/path">>, "to/my", <<"ressource">>])).

t_join_with_uri() ->
  ?assertMatch(<<"http://example.com/hello/world">>,
               bucuri:join([<<"http://example.com">>, "hello", <<"world">>])),
  ?assertMatch("http://example.com/hello/world",
               bucuri:join(["http://example.com", "hello", "world"])),
  ?assertMatch("http://example.com/hello/world/",
               bucuri:join(["http://example.com/", "/hello", "/world/"])),
  ?assertMatch("http://example.com/hello/world",
               bucuri:join(["http://example.com/", "/hello/", "/world"])).

