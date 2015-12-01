-module(buclists_tests).

-include_lib("eunit/include/eunit.hrl").

buclists_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_pipemap())
    , ?_test(t_keyfind())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_pipemap() ->
  ?assertEqual(["HELLO", "WORLD"],
               buclists:pipemap([fun atom_to_list/1,
                                 fun string:to_upper/1], [hello, world])).

t_keyfind() ->
  ?assertEqual(value, buclists:keyfind(key, 1, [{key, value}], undefined)),
  ?assertEqual(undefined, buclists:keyfind(missing_key, 1, [{key, value}], undefined)).
