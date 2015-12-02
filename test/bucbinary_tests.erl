-module(bucbinary_tests).

-include_lib("eunit/include/eunit.hrl").

bucbinary_test_() ->
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
  ?assertEqual(<<"hello,world,i,love,you">>,
               bucbinary:join([<<"hello">>, <<"world">>, <<"i">>, <<"love">>, <<"you">>],
                              <<",">>)).

