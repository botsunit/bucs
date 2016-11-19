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

bucassert_assert_call_test_() ->
  {setup,
   fun() ->
       meck:new(fake_module, [non_strict]),
       meck:expect(fake_module, function, fun(_) -> ok end),
       meck:expect(fake_module, function, fun(_, _) -> ok end)
   end,
   fun(_) ->
       meck:unload(fake_module)
   end,
   [
    fun() ->
        ?assertCall(fake_module, function, 1, 0),
        ?assertCall(fake_module, function, 0),
        fake_module:function(1),
        ?assertCall(fake_module, function, 1, 1),
        ?assertCall(fake_module, function, 1),
        fake_module:function(1),
        fake_module:function(1),
        fake_module:function(2, 2),
        ?assertCall(fake_module, function, 1, 3),
        ?assertCall(fake_module, function, 4)
    end
   ]}.
