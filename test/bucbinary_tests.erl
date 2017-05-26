-module(bucbinary_tests).

-include_lib("eunit/include/eunit.hrl").

bucbinary_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_join())
    , ?_test(t_trim())
    , ?_test(t_integers())
    , ?_test(t_floats())
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

t_integers() ->
  ?assert(bucbinary:is_integer(<<"123">>)),
  ?assert(bucbinary:is_integer(<<"1">>)),
  ?assertNot(bucbinary:is_integer(<<"A">>)),
  ?assertNot(bucbinary:is_integer(<<"1.2">>)),
  ?assert(bucbinary:are_integers([<<"1">>, <<"123">>])),
  ?assertNot(bucbinary:are_integers([<<"A">>, <<"123">>])),
  ?assertNot(bucbinary:are_integers([<<"1.2">>, <<"123">>])).

t_floats() ->
  ?assert(bucbinary:is_float(<<"12.3">>)),
  ?assertNot(bucbinary:is_float(<<"1.">>)),
  ?assertNot(bucbinary:is_float(<<".1">>)),
  ?assertNot(bucbinary:is_float(<<"A">>)),
  ?assertNot(bucbinary:is_float(<<"123">>)),
  ?assert(bucbinary:are_floats([<<"1.1">>, <<"12.3">>])),
  ?assertNot(bucbinary:are_floats([<<"A">>, <<"12.3">>])),
  ?assertNot(bucbinary:are_floats([<<"1.2">>, <<"123">>])).
