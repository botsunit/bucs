-module(bucassert_tests).

-include_lib("../include/bucassert.hrl").
-include_lib("eunit/include/eunit.hrl").

bucassert_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) -> ok end,
   [
    fun() ->
        V = {ok, 1},
        ?assertContinueIfMatch({ok, X}, V, X,
                               fun(A) ->
                                   ?assertEqual(A, 1)
                               end)
    end
   ]}.

