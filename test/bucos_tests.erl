-module(bucos_tests).

-include_lib("eunit/include/eunit.hrl").

bucos_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_return())
    , ?_test(t_timeout())
    , ?_test(t_error())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_return() ->
  ?assertMatch({ok, "Hello\n"}, bucos:run("echo Hello")).

t_timeout() ->
  ?assertMatch({error, timeout}, bucos:run("sleep 10", 10)).

t_error() ->
  ?assertMatch({error, _}, bucos:run("invalidCommand 2>&1")).

