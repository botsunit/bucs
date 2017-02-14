-module(bucbinary_tests).

-include_lib("eunit/include/eunit.hrl").

bucbinary_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_join())
    , ?_test(t_trim())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_join() ->
  ?assertEqual(<<"hello,world,i,love,you">>,
               bucbinary:join([<<"hello">>, <<"world">>, <<"i">>, <<"love">>, <<"you">>],
                              <<",">>)),
  ?assertEqual(<<"hello world i love you">>,
               bucbinary:join(["hello", world, <<"i">>, <<"love">>, <<"you">>],
                              <<" ">>)),
  ?assertEqual(<<"helloworldiloveyou">>,
               bucbinary:join([<<"hello">>, <<"world">>, <<"i">>, <<"love">>, <<"you">>],
                              <<>>)).

t_trim() ->
  ?assertEqual(<<"hello world">>,
               bucbinary:trim(<<"       hello world">>, left)),
  ?assertEqual(<<"hello world">>,
               bucbinary:trim(<<"hello world   ">>, right)),
  ?assertEqual(<<"hello world  ">>,
               bucbinary:trim(<<"       hello world  ">>, left)),
  ?assertEqual(<<"  hello world">>,
               bucbinary:trim(<<"  hello world   ">>, right)),
  ?assertEqual(<<"hello world">>,
               bucbinary:trim(<<"      hello world   ">>, both)),
  ?assertEqual(<<"hello \n\r world">>,
               bucbinary:trim(<<"  \r    hello \n\r world \n  ">>, both)).

