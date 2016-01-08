-module(bucrandom_tests).

-include_lib("eunit/include/eunit.hrl").

bucrandom_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_randstr_has_good_content()),
    ?_test(t_randstr_has_good_length()),
    ?_test(t_randstr_is_good_random()),
    ?_test(t_empty_randstr_is_empty())
   ]}.

setup() ->
  _ = application:ensure_all_started(bucs),
  ok.

teardown(_) ->
  ok.

build_randstrs(0) ->
  [];
build_randstrs(N) ->
  [bucrandom:randstr(10)|build_randstrs(N-1)].

all_are_different([]) ->
  true;
all_are_different([H|T]) ->
  not lists:member(H,T) andalso all_are_different(T).

t_randstr_has_good_content() ->
  LegalChars = "azertyuiopqsdfghjklmwxcvbnAZERTYUIOPQSDFGHJKLMWXCVBN1234567890",
  RandomStr = bucrandom:randstr(1024),
  LegalStr = [X || X <- RandomStr, lists:member(X, LegalChars)],
  ?assertEqual(LegalStr, RandomStr).

t_randstr_has_good_length() ->
  ?assertEqual(234, string:len(bucrandom:randstr(234))).

t_randstr_is_good_random() ->
  ?assert(all_are_different(build_randstrs(100))).

t_empty_randstr_is_empty() ->
  ?assertEqual([], bucrandom:randstr(0)).
