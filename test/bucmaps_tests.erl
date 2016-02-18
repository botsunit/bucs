-module(bucmaps_tests).

-include_lib("eunit/include/eunit.hrl").

bucmaps_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) -> ok end,
   [
    fun() ->
        ?assertMatch(
           #{a := b, c := d},
           bucmaps:from_list([{a, b}, {c, d}])),
        ?assertMatch(
           #{a := b, c := #{d := e}},
           bucmaps:from_list([{a, b}, {c, [{d, e}]}])),
        ?assertMatch(
           #{a := b, c := #{d := #{e := f}}},
           bucmaps:from_list([{a, b}, {c, [{d, [{e, f}]}]}])),
        ?assertMatch(
           #{a := b, c := #{d := [{e, f}]}},
           bucmaps:from_list([{a, b}, {c, [{d, [{e, f}]}]}], 2))
    end,
    fun() ->
        ?assertMatch(
           [{a, b}, {c, d}],
           bucmaps:to_list(#{a => b, c => d})),
        ?assertMatch(
           [{a, b}, {c, [{d, e}]}],
           bucmaps:to_list(#{a => b, c => #{d => e}})),
        ?assertMatch(
           [{a, b}, {c, [{d, [{e, f}]}]}],
           bucmaps:to_list(#{a => b, c => #{d => #{e => f}}})),
        ?assertMatch(
           [{a, b}, {c, [{d, [{e, f}]}]}],
           bucmaps:to_list(#{a => b, c => #{d => [{e, f}]}}, 2))
    end
   ]}.

