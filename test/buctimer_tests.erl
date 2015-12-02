-module(buctimer_tests).

-include_lib("eunit/include/eunit.hrl").

buctimer_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
      ?_test(t_verify_year())
      , ?_test(t_verify_month())
      , ?_test(t_verify_day())
      , ?_test(t_verify_hour())
      , ?_test(t_verify_minute())
      , ?_test(t_verify_second())
      , ?_test(t_next())
      , ?_test(t_next_stop())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_verify_year() ->
  ?assertMatch({error, year}, buctimer:verify({error, '*', '*', '*', '*', '*'})),
  ?assertMatch({error, year}, buctimer:verify({<<"error">>, '*', '*', '*', '*', '*'})),
  ?assertMatch({error, year}, buctimer:verify({-1, '*', '*', '*', '*', '*'})),
  ?assertMatch({error, year}, buctimer:verify({'*/x', '*', '*', '*', '*', '*'})),
  ?assertMatch({error, year}, buctimer:verify({'x', '*', '*', '*', '*', '*'})),
  ?assertMatch({error, year}, buctimer:verify({'x/*', '*', '*', '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({2000, '*', '*', '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({[2000, 2002], '*', '*', '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*/2', '*', '*', '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*', '*', '*'})).

t_verify_month() ->
  ?assertMatch({error, month}, buctimer:verify({'*', error, '*', '*', '*', '*'})),
  ?assertMatch({error, month}, buctimer:verify({'*', <<"error">>, '*', '*', '*', '*'})),
  ?assertMatch({error, month}, buctimer:verify({'*', 0, '*', '*', '*', '*'})),
  ?assertMatch({error, month}, buctimer:verify({'*', 13, '*', '*', '*', '*'})),
  ?assertMatch({error, month}, buctimer:verify({'*', '*/x', '*', '*', '*', '*'})),
  ?assertMatch({error, month}, buctimer:verify({'*', 'x', '*', '*', '*', '*'})),
  ?assertMatch({error, month}, buctimer:verify({'*', 'x/*', '*', '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', [1, 3], '*', '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', 3, '*', '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*/2', '*', '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*', '*', '*'})).

t_verify_day() ->
  ?assertMatch({error, day}, buctimer:verify({'*', '*', error, '*', '*', '*'})),
  ?assertMatch({error, day}, buctimer:verify({'*', '*', <<"error">>, '*', '*', '*'})),
  ?assertMatch({error, day}, buctimer:verify({'*', '*', 0, '*', '*', '*'})),
  ?assertMatch({error, day}, buctimer:verify({'*', '*', 32, '*', '*', '*'})),
  ?assertMatch({error, day}, buctimer:verify({'*', '*', '*/x', '*', '*', '*'})),
  ?assertMatch({error, day}, buctimer:verify({'*', '*', 'x', '*', '*', '*'})),
  ?assertMatch({error, day}, buctimer:verify({'*', '*', 'x/*', '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', monday, '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', thursday, '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', wednesday, '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', thursday, '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', friday, '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', saturday, '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', sunday, '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', [monday, sunday], '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', [3, 10], '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', 3, '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*/2', '*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*', '*', '*'})).

t_verify_hour() ->
  ?assertMatch({error, hour}, buctimer:verify({'*', '*', '*', error, '*', '*'})),
  ?assertMatch({error, hour}, buctimer:verify({'*', '*', '*', <<"error">>, '*', '*'})),
  ?assertMatch({error, hour}, buctimer:verify({'*', '*', '*', -1, '*', '*'})),
  ?assertMatch({error, hour}, buctimer:verify({'*', '*', '*', 24, '*', '*'})),
  ?assertMatch({error, hour}, buctimer:verify({'*', '*', '*', '*/x', '*', '*'})),
  ?assertMatch({error, hour}, buctimer:verify({'*', '*', '*', 'x', '*', '*'})),
  ?assertMatch({error, hour}, buctimer:verify({'*', '*', '*', 'x/*', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', [3, 7], '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', 2, '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*/2', '*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*', '*', '*'})).

t_verify_minute() ->
  ?assertMatch({error, minute}, buctimer:verify({'*', '*', '*', '*', error, '*'})),
  ?assertMatch({error, minute}, buctimer:verify({'*', '*', '*', '*', <<"error">>, '*'})),
  ?assertMatch({error, minute}, buctimer:verify({'*', '*', '*', '*', -1, '*'})),
  ?assertMatch({error, minute}, buctimer:verify({'*', '*', '*', '*', 60, '*'})),
  ?assertMatch({error, minute}, buctimer:verify({'*', '*', '*', '*', '*/x', '*'})),
  ?assertMatch({error, minute}, buctimer:verify({'*', '*', '*', '*', 'x', '*'})),
  ?assertMatch({error, minute}, buctimer:verify({'*', '*', '*', '*', 'x/*', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*', [3, 7], '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*', 2, '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*', '*/2', '*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*', '*', '*'})).

t_verify_second() ->
  ?assertMatch({error, second}, buctimer:verify({'*', '*', '*', '*', '*', error})),
  ?assertMatch({error, second}, buctimer:verify({'*', '*', '*', '*', '*', <<"error">>})),
  ?assertMatch({error, second}, buctimer:verify({'*', '*', '*', '*', '*', -1})),
  ?assertMatch({error, second}, buctimer:verify({'*', '*', '*', '*', '*', 60})),
  ?assertMatch({error, second}, buctimer:verify({'*', '*', '*', '*', '*', '*/x'})),
  ?assertMatch({error, second}, buctimer:verify({'*', '*', '*', '*', '*', 'x'})),
  ?assertMatch({error, second}, buctimer:verify({'*', '*', '*', '*', '*', 'x/*'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*', '*', [3, 7]})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*', '*', 2})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*', '*', '*/2'})),
  ?assertMatch(ok, buctimer:verify({'*', '*', '*', '*', '*', '*'})).

t_next() ->
  ?assertMatch({ok, {{2000, 1, 1}, {0, 0, 1}}, 1}, 
               buctimer:next({2000, '*', '*', '*', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 3}, {0, 0, 0}}, 60*60*24*2}, 
               buctimer:next({2000, '*', monday, '*', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {0, 0, 1}}, 1}, 
               buctimer:next({2000, '*', [saturday, monday], '*', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {0, 0, 1}}, 1}, 
               buctimer:next({2000, 1, 1, 0, 0, 1},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {0, 1, 0}}, 60}, 
               buctimer:next({2000, 1, 1, 0, 1, 0},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {1, 0, 0}}, 60*60}, 
               buctimer:next({2000, 1, 1, 1, 0, 0},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 2}, {0, 0, 0}}, 60*60*24}, 
               buctimer:next({2000, 1, 2, 0, 0, 0},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 2, 1}, {0, 0, 0}}, 60*60*24*31}, 
               buctimer:next({2000, 2, 1, 0, 0, 0},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 3, 1}, {0, 0, 0}}, 60*60*24*(31+29)}, 
               buctimer:next({2000, 3, 1, 0, 0, 0},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2001, 3, 1}, {0, 0, 0}}, 60*60*24*(31+28)}, 
               buctimer:next({2001, 3, 1, 0, 0, 0},
                             {2001, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {0, 0, 5}}, 5}, 
               buctimer:next({2000, '*', '*', '*', '*', '*/5'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {0, 5, 0}}, 5*60}, 
               buctimer:next({2000, '*', '*', '*', '*/5', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {5, 0, 0}}, 5*60*60}, 
               buctimer:next({2000, '*', '*', '*/5', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 6}, {0, 0, 0}}, 5*60*60*24}, 
               buctimer:next({2000, '*', '*/5', '*', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
   ?assertMatch({ok, {{2000, 1, 17}, {0, 0, 0}}, 16*60*60*24}, 
                buctimer:next({2000, '*', '*/16', '*', '*', '*'},
                              {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 6, 1}, {0, 0, 0}}, (31+29+31+30+31)*60*60*24}, 
               buctimer:next({2000, '*/5', '*', '*', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2002, 1, 1}, {0, 0, 0}}, _}, 
               buctimer:next({'*/2', '*', '*', '*', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})).

t_next_stop() ->
  ?assertMatch(stop, buctimer:next({2000, '*', '*', '*', '*', '*'})).

