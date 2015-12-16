-module(bucstring_tests).

-include_lib("eunit/include/eunit.hrl").

bucstring_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_sub())
    , ?_test(t_gsub())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_sub() ->
  ?assertEqual("My name is Bob... Sponge Bill!",
               bucstring:sub("My name is Bill... Sponge Bill!",
                             "Bill",
                             "Bob")).

t_gsub() ->
  ?assertEqual("My name is Bob... Sponge Bob!",
               bucstring:gsub("My name is Bill... Sponge Bill!",
                              "Bill",
                              "Bob")).

