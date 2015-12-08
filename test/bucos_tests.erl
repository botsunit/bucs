-module(bucos_tests).

-include_lib("eunit/include/eunit.hrl").

bucos_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_return())
    , ?_test(t_timeout())
    , ?_test(t_error())
    , ?_test(t_error_with_data())
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
  ?assertMatch({error, _, _ }, bucos:run("invalidCommand 2>&1")).

t_error_with_data() ->
  {error,ErrCode,Data} = bucos:run("invalidCommand 2>&1"),
  ?assert(ErrCode /= 0 andalso string:len(Data)>14). % >14: I assume that the faulty command is somewhere inside the error output
