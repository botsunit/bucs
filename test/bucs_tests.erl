-module(bucs_tests).
-export([addition/2]).

-include("../include/bucs.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/bucassert.hrl").

-record(rec, {bar = "baz", camp = "spam"}).

bucs_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_convert())
    , ?_test(t_module_exists())
    , ?_test(t_module_doesnt_exists())
    , ?_test(t_function_exists())
    , ?_test(t_function_doesnt_exists_in_module())
    , ?_test(t_function_doesnt_exists_cause_bad_module())
    , ?_test(t_function_doesnt_exists_cause_private())
    , ?_test(t_apply())
    , ?_test(t_is())
    , ?_test(t_convert_record())
    , ?_test(t_blank())
    , ?_test(t_eval())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_convert() ->
  ?assertEqual(atom, bucs:to_atom(atom)),
  ?assertEqual(atom, bucs:to_atom(<<"atom">>)),
  ?assertEqual(atom, bucs:to_atom("atom")),
  ?assertEqual('<0.0.0>', bucs:to_atom(c:pid(0, 0, 0))),
  ?assertEqual(atom, bucs:to(atom, atom)),
  ?assertEqual(atom, bucs:to(atom, <<"atom">>)),
  ?assertEqual(atom, bucs:to(atom, "atom")),
  ?assertEqual('<0.0.0>', bucs:to(atom, c:pid(0, 0, 0))),
  ?assertEqual(atom, bucs:as(atom, atom)),
  ?assertEqual(atom, bucs:as(atom, <<"atom">>)),
  ?assertEqual(atom, bucs:as(atom, "atom")),
  ?assertEqual('<0.0.0>', bucs:as(atom, c:pid(0, 0, 0))),

  ?assertEqual("list", bucs:to_list(list)),
  ?assertEqual("list", bucs:to_list("list")),
  ?assertEqual("list", bucs:to_list(<<"list">>)),
  ?assertEqual("123", bucs:to_list(123)),
  ?assertEqual("1.20000000000000000000e+01", bucs:to_list(12.0)),
  ?assertEqual("true", bucs:to_list(true)),
  ?assertEqual("false", bucs:to_list(false)),
  ?assertEqual("<0.0.0>", bucs:to_list(c:pid(0, 0, 0))),
  ?assertEqual("list", bucs:to(list, list)),
  ?assertEqual("list", bucs:to(list, "list")),
  ?assertEqual("list", bucs:to(list, <<"list">>)),
  ?assertEqual("123", bucs:to(list, 123)),
  ?assertEqual("1.20000000000000000000e+01", bucs:to(list, 12.0)),
  ?assertEqual("true", bucs:to(list, true)),
  ?assertEqual("false", bucs:to(list, false)),
  ?assertEqual("<0.0.0>", bucs:to(list, c:pid(0, 0, 0))),
  ?assertEqual("list", bucs:as([1, 2, 3], list)),
  ?assertEqual("list", bucs:as([1, 2, 3], "list")),
  ?assertEqual("list", bucs:as([1, 2, 3], <<"list">>)),
  ?assertEqual("123", bucs:as([1, 2, 3], 123)),
  ?assertEqual("1.20000000000000000000e+01", bucs:as([1, 2, 3], 12.0)),
  ?assertEqual("true", bucs:as([1, 2, 3], true)),
  ?assertEqual("false", bucs:as([1, 2, 3], false)),
  ?assertEqual("<0.0.0>", bucs:as([1, 2, 3], c:pid(0, 0, 0))),

  ?assertEqual(<<"list">>, bucs:to_binary(list)),
  ?assertEqual(<<"list">>, bucs:to_binary("list")),
  ?assertEqual(<<"list">>, bucs:to_binary(<<"list">>)),
  ?assertEqual(<<"123">>, bucs:to_binary(123)),
  ?assertEqual(<<"12.0">>, bucs:to_binary(12.0)),
  ?assertEqual(<<"true">>, bucs:to_binary(true)),
  ?assertEqual(<<"false">>, bucs:to_binary(false)),
  ?assertEqual(<<"<0.0.0>">>, bucs:to_binary(c:pid(0, 0, 0))),
  ?assertEqual(<<"list">>, bucs:to(binary, list)),
  ?assertEqual(<<"list">>, bucs:to(binary, "list")),
  ?assertEqual(<<"list">>, bucs:to(binary, <<"list">>)),
  ?assertEqual(<<"123">>, bucs:to(binary, 123)),
  ?assertEqual(<<"12.0">>, bucs:to(binary, 12.0)),
  ?assertEqual(<<"true">>, bucs:to(binary, true)),
  ?assertEqual(<<"false">>, bucs:to(binary, false)),
  ?assertEqual(<<"<0.0.0>">>, bucs:to(binary, c:pid(0, 0, 0))),
  ?assertEqual(<<"list">>, bucs:as(<<>>, list)),
  ?assertEqual(<<"list">>, bucs:as(<<>>, "list")),
  ?assertEqual(<<"list">>, bucs:as(<<>>, <<"list">>)),
  ?assertEqual(<<"123">>, bucs:as(<<>>, 123)),
  ?assertEqual(<<"12.0">>, bucs:as(<<>>, 12.0)),
  ?assertEqual(<<"true">>, bucs:as(<<>>, true)),
  ?assertEqual(<<"false">>, bucs:as(<<>>, false)),
  ?assertEqual(<<"<0.0.0>">>, bucs:as(<<>>, c:pid(0, 0, 0))),

  ?assertEqual(123, bucs:to_integer(123)),
  ?assertEqual(123, bucs:to_integer("123")),
  ?assertEqual(123, bucs:to_integer("123.444")),
  ?assertEqual(124, bucs:to_integer("123.445")),
  ?assertEqual(123, bucs:to_integer(<<"123">>)),
  ?assertEqual(123, bucs:to_integer(<<"123.444">>)),
  ?assertEqual(124, bucs:to_integer(<<"123.445">>)),
  ?assertEqual(123, bucs:to_integer('123')),
  ?assertEqual(123, bucs:to_integer('123.444')),
  ?assertEqual(124, bucs:to_integer('123.445')),
  ?assertEqual(123, bucs:to_integer(123.444)),
  ?assertEqual(124, bucs:to_integer(123.445)),
  ?assertEqual(123, bucs:to(integer, 123)),
  ?assertEqual(123, bucs:to(integer, "123")),
  ?assertEqual(123, bucs:to(integer, "123.444")),
  ?assertEqual(124, bucs:to(integer, "123.445")),
  ?assertEqual(123, bucs:to(integer, <<"123">>)),
  ?assertEqual(123, bucs:to(integer, <<"123.444">>)),
  ?assertEqual(124, bucs:to(integer, <<"123.445">>)),
  ?assertEqual(123, bucs:to(integer, '123')),
  ?assertEqual(123, bucs:to(integer, '123.444')),
  ?assertEqual(124, bucs:to(integer, '123.445')),
  ?assertEqual(123, bucs:to(integer, 123.444)),
  ?assertEqual(124, bucs:to(integer, 123.445)),
  ?assertEqual(123, bucs:as(123, 123)),
  ?assertEqual(123, bucs:as(123, "123")),
  ?assertEqual(123, bucs:as(123, "123.444")),
  ?assertEqual(124, bucs:as(123, "123.445")),
  ?assertEqual(123, bucs:as(123, <<"123">>)),
  ?assertEqual(123, bucs:as(123, <<"123.444">>)),
  ?assertEqual(124, bucs:as(123, <<"123.445">>)),
  ?assertEqual(123, bucs:as(123, '123')),
  ?assertEqual(123, bucs:as(123, '123.444')),
  ?assertEqual(124, bucs:as(123, '123.445')),
  ?assertEqual(123, bucs:as(123, 123.444)),
  ?assertEqual(124, bucs:as(123, 123.445)),

  ?assertEqual(123.45, bucs:to_float(123.45)),
  ?assertEqual(123.45, bucs:to_float("123.45")),
  ?assertEqual(123.45, bucs:to_float(<<"123.45">>)),
  ?assertEqual(123.45, bucs:to_float('123.45')),
  ?assertEqual(123.0, bucs:to_float(123)),
  ?assertEqual(123.457, bucs:to_float(123.45678, 3)),
  ?assertEqual(123.457, bucs:to_float("123.45678", 3)),
  ?assertEqual(123.457, bucs:to_float(<<"123.45678">>, 3)),
  ?assertEqual(123.457, bucs:to_float('123.45678', 3)),
  ?assertEqual(123.0, bucs:to_float(123, 3)),
  ?assertEqual(123.45, bucs:to(float, 123.45)),
  ?assertEqual(123.45, bucs:to(float, "123.45")),
  ?assertEqual(123.45, bucs:to(float, <<"123.45">>)),
  ?assertEqual(123.45, bucs:to(float, '123.45')),
  ?assertEqual(123.0, bucs:to(float, 123)),
  ?assertEqual(123.45, bucs:as(123.45, 123.45)),
  ?assertEqual(123.45, bucs:as(123.45, "123.45")),
  ?assertEqual(123.45, bucs:as(123.45, <<"123.45">>)),
  ?assertEqual(123.45, bucs:as(123.45, '123.45')),
  ?assertEqual(123.0, bucs:as(123.45, 123)),

  ?assertEqual("hello", bucs:to_string(hello)),
  ?assertEqual("hello.world", bucs:to_string('hello.world')),
  ?assertEqual("hello", bucs:to_string("hello")),
  ?assertEqual("hello", bucs:to_string(<<"hello">>)),
  ?assertEqual("[1,2,3,4]", bucs:to_string([1, 2, 3, 4])),
  ?assertEqual("{1,2,3,4}", bucs:to_string({1, 2, 3, 4})),
  ?assertEqual("123", bucs:to_string(123)),
  ?assertEqual("123.4", bucs:to_string(123.4)),
  ?assertEqual("hello", bucs:to(string, hello)),
  ?assertEqual("hello.world", bucs:to(string, 'hello.world')),
  ?assertEqual("hello", bucs:to(string, "hello")),
  ?assertEqual("hello", bucs:to(string, <<"hello">>)),
  ?assertEqual("[1,2,3,4]", bucs:to(string, [1, 2, 3, 4])),
  ?assertEqual("{1,2,3,4}", bucs:to(string, {1, 2, 3, 4})),
  ?assertEqual("123", bucs:to(string, 123)),
  ?assertEqual("123.4", bucs:to(string, 123.4)),
  ?assertEqual("hello", bucs:as("string", hello)),
  ?assertEqual("hello.world", bucs:as("string", 'hello.world')),
  ?assertEqual("hello", bucs:as("string", "hello")),
  ?assertEqual("hello", bucs:as("string", <<"hello">>)),
  ?assertEqual("[1,2,3,4]", bucs:as("string", [1, 2, 3, 4])),
  ?assertEqual("{1,2,3,4}", bucs:as("string", {1, 2, 3, 4})),
  ?assertEqual("123", bucs:as("string", 123)),
  ?assertEqual("123.4", bucs:as("string", 123.4)),

  ?assertEqual({ok, 123}, bucs:to_term(123)),
  ?assertEqual({ok, 123}, bucs:to_term(<<"123">>)),
  ?assertEqual({ok, 123}, bucs:to_term("123")),
  ?assertEqual({ok, atom}, bucs:to_term(atom)),
  ?assertEqual({ok, atom}, bucs:to_term("atom")),
  ?assertEqual({ok, atom}, bucs:to_term(<<"atom">>)),
  ?assertEqual({ok, "string"}, bucs:to_term("\"string\"")),
  ?assertEqual({ok, "string"}, bucs:to_term(<<"\"string\"">>)),
  ?assertEqual({ok, [1, 2, 3, 4]}, bucs:to_term("[1, 2, 3, 4]")),
  ?assertEqual({ok, {hello, 123.45, 678, "hello", <<"world">>, [1, 2, 3, 4]}},
               bucs:to_term("{hello, 123.45, 678, \"hello\", <<\"world\">>, [1, 2, 3, 4]}")),
  ?assertEqual({ok, {1, {2, {3, {4}}}}}, bucs:to_term("{1, {2, {3, {4}}}}")),
  ?assertEqual({ok, {hello, 123.45, 678, "hello", <<"world">>, [1, 2, 3, 4]}},
               bucs:to_term({hello, 123.45, 678, "hello", <<"world">>, [1, 2, 3, 4]})),
  ?assertEqual({ok, {1, {2, {3, {4}}}}}, bucs:to_term({1, {2, {3, {4}}}})),
  ?assertEqual({ok, 123}, bucs:to(term, 123)),
  ?assertEqual({ok, 123}, bucs:to(term, <<"123">>)),
  ?assertEqual({ok, 123}, bucs:to(term, "123")),
  ?assertEqual({ok, atom}, bucs:to(term, atom)),
  ?assertEqual({ok, atom}, bucs:to(term, "atom")),
  ?assertEqual({ok, atom}, bucs:to(term, <<"atom">>)),
  ?assertEqual({ok, "string"}, bucs:to(term, "\"string\"")),
  ?assertEqual({ok, "string"}, bucs:to(term, <<"\"string\"">>)),
  ?assertEqual({ok, [1, 2, 3, 4]}, bucs:to(term, "[1, 2, 3, 4]")),
  ?assertEqual({ok, {hello, 123.45, 678, "hello", <<"world">>, [1, 2, 3, 4]}},
               bucs:to(term, "{hello, 123.45, 678, \"hello\", <<\"world\">>, [1, 2, 3, 4]}")),
  ?assertEqual({ok, {1, {2, {3, {4}}}}}, bucs:to(term, "{1, {2, {3, {4}}}}")),
  ?assertEqual({ok, {hello, 123.45, 678, "hello", <<"world">>, [1, 2, 3, 4]}},
               bucs:to(term, {hello, 123.45, 678, "hello", <<"world">>, [1, 2, 3, 4]})),
  ?assertEqual({ok, {1, {2, {3, {4}}}}}, bucs:to(term, {1, {2, {3, {4}}}})),

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

t_module_exists() ->
  ?assert(bucs:module_exists(bucs)).

t_module_doesnt_exists() ->
  ?assertNot(bucs:module_exists(this_module_surely_doesnt_exists)).

t_function_exists() ->
  ?assert(bucs:function_exists(bucs, function_exists, 3)).

t_function_doesnt_exists_in_module() ->
  ?assertNot(bucs:function_exists(bucs, this_function_surely_doesnt_exists, 12)).

t_function_doesnt_exists_cause_bad_module() ->
  ?assertNot(bucs:function_exists(this_module_surely_doesnt_exists, function_exists, 3)).

t_function_doesnt_exists_cause_private() ->
  ?assertNot(bucs:function_exists(bucs, compare_as, 3)).

t_apply() ->
  ?assertEqual({ok, 5}, bucs:apply(fun addition/2, [3, 2])),
  ?assertEqual({ok, 21}, bucs:apply(fun multiplication/2, [3, 7])),
  ?assertEqual(error, bucs:apply(fun undef:missing/2, [3, 7])),
  ?assertEqual(missing, bucs:apply(fun undef:missing/2, [3, 7], missing)),
  ?assertEqual({ok, 5}, bucs:apply(?MODULE, addition, [2, 3])),
  ?assertEqual(error, bucs:apply(?MODULE, missing_function, [2, 3])),
  ?assertEqual(missing, bucs:apply(?MODULE, missing_function, [2, 3], missing)),
  ?assertEqual(5, bucs:apply(?MODULE, addition, [2, 3], error)).

t_is() ->
  ?assertNot(bucs:is_string([1, 2, 3])),
  ?assertNot(bucs:is_string(hello)),
  ?assert(bucs:is_string("Hello World")),
  ?assertNot(bucs:is_string(["hello world"])),
  ?assertNot(bucs:is_kw_list("hello")),
  ?assertNot(bucs:is_kw_list([1, 2, 3])),
  ?assertNot(bucs:is_kw_list([{a, b}, {c, d}, {e, f, g}])),
  ?assert(bucs:is_kw_list([{a, b}, {c, d}, {e, f}])),
  ?assert(bucs:is_list_of_lists([[], [], []])),
  ?assertNot(bucs:is_list_of_lists([a, b, c])),
  ?assertNot(bucs:is_list_of_lists("hello")).

t_convert_record() ->
  ?assertMatch([{bar, "baz"}, {camp, "spam"}], ?record_to_list(rec, #rec{})),
  ?assertMatch(#{bar := "baz", camp := "spam"}, ?record_to_map(rec, #rec{})),
  ?assertMatch(#rec{bar = "baz", camp = "spam"}, ?list_to_record(rec, [{bar, "baz"}, {camp, "spam"}])),
  ?assertMatch(#rec{bar = "baz", camp = "spam"}, ?map_to_record(rec, #{bar => "baz", camp => "spam"})).

-record(test, {foo, bar, baz}).

t_blank() ->
  ?assert(bucs:blank([])),
  ?assert(bucs:blank({})),
  ?assert(bucs:blank("")),
  ?assert(bucs:blank(#{})),
  ?assert(bucs:blank(<<"">>)),
  ?assert(bucs:blank("  ")),
  ?assert(bucs:blank(<<"    ">>)),
  ?assert(bucs:blank(nil)),
  ?assert(bucs:blank(undefined)),
  ?assert(bucs:blank(false)),
  ?assertNot(bucs:blank(0)),
  ?assertNot(bucs:blank(#{toto => titi})),
  ?assertNot(bucs:blank({hello})),
  ?assertNot(bucs:blank(#test{})).

t_eval() ->
  ?assertContinueIfMatch({value, Fun, _}, bucs:eval("fun(X) -> X * X end"), Fun,
                         fun(F) ->
                             ?assertEqual(25, F(5))
                         end).
