-module(bucs_tests).

-include_lib("eunit/include/eunit.hrl").

bucs_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_convert())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_convert() ->
  ?assertEqual(atom, bucs:to_atom(atom)),
  ?assertEqual(atom, bucs:to_atom(<<"atom">>)),
  ?assertEqual(atom, bucs:to_atom("atom")),
  ?assertEqual("list", bucs:to_list(list)),
  ?assertEqual("list", bucs:to_list("list")),
  ?assertEqual("list", bucs:to_list(<<"list">>)),
  ?assertEqual("123", bucs:to_list(123)),
  ?assertEqual("1.20000000000000000000e+01", bucs:to_list(12.0)),
  ?assertEqual("true", bucs:to_list(true)),
  ?assertEqual("false", bucs:to_list(false)),
  ?assertEqual(<<"list">>, bucs:to_binary(list)),
  ?assertEqual(<<"list">>, bucs:to_binary("list")),
  ?assertEqual(<<"list">>, bucs:to_binary(<<"list">>)),
  ?assertEqual(<<"123">>, bucs:to_binary(123)),
  ?assertEqual(<<"1.20000000000000000000e+01">>, bucs:to_binary(12.0)),
  ?assertEqual(<<"true">>, bucs:to_binary(true)),
  ?assertEqual(<<"false">>, bucs:to_binary(false)),
  ?assertEqual(123, bucs:to_integer(123)),
  ?assertEqual(123, bucs:to_integer("123")),
  ?assertEqual(123, bucs:to_integer(<<"123">>)),
  ?assertEqual(123, bucs:to_integer('123')),
  ?assertEqual(123, bucs:to_integer(123.444)),
  ?assertEqual(124, bucs:to_integer(123.445)),
  ?assertEqual(123.45, bucs:to_float(123.45)),
  ?assertEqual(123.45, bucs:to_float("123.45")),
  ?assertEqual(123.45, bucs:to_float(<<"123.45">>)),
  ?assertEqual(123.45, bucs:to_float('123.45')),
  ?assertEqual(123.0, bucs:to_float(123)),
  ?assertEqual(bucs:to_binary(
                 bucs:to_list(
                   bucs:to_atom(123.0))), 
               bucs:pipecall([
                              {fun bucs:to_atom/1, [123.0]},
                              fun bucs:to_list/1,
                              fun bucs:to_binary/1
                             ])),
  ?assertEqual(addition(multiplication(math:log(math:pow(7, 3)), 7), 7),
               bucs:pipecall([
                              {fun math:pow/2, [7, 3]},
                              fun math:log/1,
                              {fun multiplication/2, [7]},
                              {fun addition/2, [7]}
                             ])).

addition(A, B) -> A + B.
multiplication(A, B) -> A * B.

