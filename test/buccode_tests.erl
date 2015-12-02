-module(buccode_tests).

-include_lib("eunit/include/eunit.hrl").

buccode_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_priv_dir())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_priv_dir() ->
  ?assertMatch({match, _}, re:run(buccode:priv_dir(bucs), "bucs/priv$")).

