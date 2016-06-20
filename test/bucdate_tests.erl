-module(bucdate_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DATE, {{2001,3,10},{17,16,17}}).
-define(DATEMS, {{2001,3,10},{17,16,17,123456}}).
-define(DATE_NOON, {{2001,3,10},{12,0,0}}).
-define(DATE_MIDNIGHT, {{2001,3,10},{0,0,0}}).
-define(ISO, "o \\WW").

bucdate_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_add())
    , ?_test(t_basic_format())
    , ?_test(t_basic_parse())
    , ?_test(t_parse_with_days())
    , ?_test(t_parse_with_TZ())
    , ?_test(t_iso())
    , ?_test(t_ms())
    , ?_test(t_zulu())
    , ?_test(t_timezone())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_add() ->
  ?assertMatch({{2015,12,3},{20,15,25}},
               bucdate:add({{2015,12,3},{18,15,25}}, 2, hours)).

t_basic_format() ->
  ?assertEqual(bucdate:format("F j, Y, g:i a",?DATE), "March 10, 2001, 5:16 pm"),
  ?assertEqual(bucdate:format("F jS, Y, g:i a",?DATE), "March 10th, 2001, 5:16 pm"),
  ?assertEqual(bucdate:format("F jS",{{2011,3,21},{0,0,0}}), "March 21st"),
  ?assertEqual(bucdate:format("F jS",{{2011,3,22},{0,0,0}}), "March 22nd"),
  ?assertEqual(bucdate:format("F jS",{{2011,3,23},{0,0,0}}), "March 23rd"),
  ?assertEqual(bucdate:format("F jS",{{2011,3,31},{0,0,0}}), "March 31st"),
  ?assertEqual(bucdate:format("m.d.y",?DATE), "03.10.01"),
  ?assertEqual(bucdate:format("j, n, Y",?DATE), "10, 3, 2001"),
  ?assertEqual(bucdate:format("Ymd",?DATE), "20010310"),
  ?assertEqual(bucdate:format("H:i:s",?DATE), "17:16:17"),
  ?assertEqual(bucdate:format("z",?DATE), "68"),
  ?assertEqual(bucdate:format("D M j G:i:s Y",?DATE), "Sat Mar 10 17:16:17 2001"),
  ?assertEqual(bucdate:format("ga",?DATE_NOON), "12pm"),
  ?assertEqual(bucdate:format("gA",?DATE_NOON), "12PM"),
  ?assertEqual(bucdate:format("ga",?DATE_MIDNIGHT), "12am"),
  ?assertEqual(bucdate:format("gA",?DATE_MIDNIGHT), "12AM"),
  ?assertEqual(bucdate:format("h-i-s, j-m-y, it is w Day",?DATE),
               "05-16-17, 10-03-01, 1631 1617 6 Satpm01"),
  ?assertEqual(bucdate:format("\\i\\t \\i\\s \\t\\h\\e\\ jS \\d\\a\\y.",?DATE),
               "it is the 10th day."),
  ?assertEqual(bucdate:format("H:m:s \\m \\i\\s \\m\\o\\n\\t\\h",?DATE),
               "17:03:17 m is month").

t_basic_parse() ->
  ?assertEqual({{2008,8,22}, {17,16,17}},
               bucdate:parse("22nd of August 2008", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,0,0}},
               bucdate:parse("22-Aug-2008 6 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("22-Aug-2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,12}},
               bucdate:parse("22-Aug-2008 6:35:12 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,0,0}},
               bucdate:parse("August/22/2008 6 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("August/22/2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("22 August 2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,0,0}},
               bucdate:parse("22 Aug 2008 6AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("22 Aug 2008 6:35AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("22 Aug 2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,0,0}},
               bucdate:parse("22 Aug 2008 6", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("22 Aug 2008 6:35", ?DATE)),
  ?assertEqual({{2008,8,22}, {18,35,0}},
               bucdate:parse("22 Aug 2008 6:35 PM", ?DATE)),
  ?assertEqual({{2008,8,22}, {18,0,0}},
               bucdate:parse("22 Aug 2008 6 PM", ?DATE)),
  ?assertEqual({{2008,8,22}, {18,0,0}},
               bucdate:parse("Aug 22, 2008 6 PM", ?DATE)),
  ?assertEqual({{2008,8,22}, {18,0,0}},
               bucdate:parse("August 22nd, 2008 6:00 PM", ?DATE)),
  ?assertEqual({{2008,8,22}, {18,15,15}},
               bucdate:parse("August 22nd 2008, 6:15:15pm", ?DATE)),
  ?assertEqual({{2008,8,22}, {18,15,15}},
               bucdate:parse("August 22nd, 2008, 6:15:15pm", ?DATE)),
  ?assertEqual({{2008,8,22}, {18,15,0}},
               bucdate:parse("Aug 22nd 2008, 18:15", ?DATE)),
  ?assertEqual({{2008,8,2}, {17,16,17}},
               bucdate:parse("2nd of August 2008", ?DATE)),
  ?assertEqual({{2008,8,2}, {17,16,17}},
               bucdate:parse("August 2nd, 2008", ?DATE)),
  ?assertEqual({{2008,8,2}, {17,16,17}},
               bucdate:parse("2nd  August, 2008", ?DATE)),
  ?assertEqual({{2008,8,2}, {17,16,17}},
               bucdate:parse("2008 August 2nd", ?DATE)),
  ?assertEqual({{2008,8,2}, {6,0,0}},
               bucdate:parse("2-Aug-2008 6 AM", ?DATE)),
  ?assertEqual({{2008,8,2}, {6,35,0}},
               bucdate:parse("2-Aug-2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,2}, {6,35,12}},
               bucdate:parse("2-Aug-2008 6:35:12 AM", ?DATE)),
  ?assertEqual({{2008,8,2}, {6,0,0}},
               bucdate:parse("August/2/2008 6 AM", ?DATE)),
  ?assertEqual({{2008,8,2}, {6,35,0}},
               bucdate:parse("August/2/2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,2}, {6,35,0}},
               bucdate:parse("2 August 2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,2}, {6,0,0}},
               bucdate:parse("2 Aug 2008 6AM", ?DATE)),
  ?assertEqual({{2008,8,2}, {6,35,0}},
               bucdate:parse("2 Aug 2008 6:35AM", ?DATE)),
  ?assertEqual({{2008,8,2}, {6,35,0}},
               bucdate:parse("2 Aug 2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,2}, {6,0,0}},
               bucdate:parse("2 Aug 2008 6", ?DATE)),
  ?assertEqual({{2008,8,2}, {6,35,0}},
               bucdate:parse("2 Aug 2008 6:35", ?DATE)),
  ?assertEqual({{2008,8,2}, {18,35,0}},
               bucdate:parse("2 Aug 2008 6:35 PM", ?DATE)),
  ?assertEqual({{2008,8,2}, {18,0,0}},
               bucdate:parse("2 Aug 2008 6 PM", ?DATE)),
  ?assertEqual({{2008,8,2}, {18,0,0}},
               bucdate:parse("Aug 2, 2008 6 PM", ?DATE)),
  ?assertEqual({{2008,8,2}, {18,0,0}},
               bucdate:parse("August 2nd, 2008 6:00 PM", ?DATE)),
  ?assertEqual({{2008,8,2}, {18,15,15}},
               bucdate:parse("August 2nd 2008, 6:15:15pm", ?DATE)),
  ?assertEqual({{2008,8,2}, {18,15,15}},
               bucdate:parse("August 2nd, 2008, 6:15:15pm", ?DATE)),
  ?assertEqual({{2008,8,2}, {18,15,0}},
               bucdate:parse("Aug 2nd 2008, 18:15", ?DATE)),
  ?assertEqual({{2012,12,10}, {0,0,0}},
               bucdate:parse("Dec 10th, 2012, 12:00 AM", ?DATE)),
  ?assertEqual({{2012,12,10}, {0,0,0}},
               bucdate:parse("10 Dec 2012 12:00 AM", ?DATE)),
  ?assertEqual({{2001,3,10}, {11,15,0}},
               bucdate:parse("11:15", ?DATE)),
  ?assertEqual({{2001,3,10}, {1,15,0}},
               bucdate:parse("1:15", ?DATE)),
  ?assertEqual({{2001,3,10}, {1,15,0}},
               bucdate:parse("1:15 am", ?DATE)),
  ?assertEqual({{2001,3,10}, {0,15,0}},
               bucdate:parse("12:15 am", ?DATE)),
  ?assertEqual({{2001,3,10}, {12,15,0}},
               bucdate:parse("12:15 pm", ?DATE)),
  ?assertEqual({{2001,3,10}, {3,45,39}},
               bucdate:parse("3:45:39", ?DATE)),
  ?assertEqual({{1963,4,23}, {17,16,17}},
               bucdate:parse("23-4-1963", ?DATE)),
  ?assertEqual({{1963,4,23}, {17,16,17}},
               bucdate:parse("23-april-1963", ?DATE)),
  ?assertEqual({{1963,4,23}, {17,16,17}},
               bucdate:parse("23-apr-1963", ?DATE)),
  ?assertEqual({{1963,4,23}, {17,16,17}},
               bucdate:parse("4/23/1963", ?DATE)),
  ?assertEqual({{1963,4,23}, {17,16,17}},
               bucdate:parse("april/23/1963", ?DATE)),
  ?assertEqual({{1963,4,23}, {17,16,17}},
               bucdate:parse("apr/23/1963", ?DATE)),
  ?assertEqual({{1963,4,23}, {17,16,17}},
               bucdate:parse("1963/4/23", ?DATE)),
  ?assertEqual({{1963,4,23}, {17,16,17}},
               bucdate:parse("1963/april/23", ?DATE)),
  ?assertEqual({{1963,4,23}, {17,16,17}},
               bucdate:parse("1963/apr/23", ?DATE)),
  ?assertEqual({{1963,4,23}, {17,16,17}},
               bucdate:parse("1963-4-23", ?DATE)),
  ?assertEqual({{1963,4,23}, {17,16,17}},
               bucdate:parse("1963-4-23", ?DATE)),
  ?assertEqual({{1963,4,23}, {17,16,17}},
               bucdate:parse("1963-apr-23", ?DATE)),
  ?assertThrow({bucdate, {bad_date, "23/ap/195"}},
               bucdate:parse("23/ap/195", ?DATE)),
  ?assertEqual({{2001,3,10}, {6,45,0}},
               bucdate:parse("6:45 am", ?DATE)),
  ?assertEqual({{2001,3,10}, {18,45,0}},
               bucdate:parse("6:45 PM", ?DATE)),
  ?assertEqual({{2001,3,10}, {18,45,0}},
               bucdate:parse("6:45 PM ", ?DATE)).

t_parse_with_days() ->
  ?assertEqual({{2008,8,22}, {17,16,17}},
               bucdate:parse("Sat 22nd of August 2008", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("Sat, 22-Aug-2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,12}},
               bucdate:parse("Sunday 22-Aug-2008 6:35:12 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("Sun 22-Aug-2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("THURSDAY, 22-August-2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {18,0,0}},
               bucdate:parse("THURSDAY, 22-August-2008 6 pM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("THU 22 August 2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("FRi 22 Aug 2008 6:35AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,0,0}},
               bucdate:parse("FRi 22 Aug 2008 6AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("Wednesday 22 Aug 2008 6:35 AM", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,35,0}},
               bucdate:parse("Monday 22 Aug 2008 6:35", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,0,0}},
               bucdate:parse("Monday 22 Aug 2008 6", ?DATE)),
  ?assertEqual({{2008,8,22}, {18,0,0}},
               bucdate:parse("Monday 22 Aug 2008 6p", ?DATE)),
  ?assertEqual({{2008,8,22}, {6,0,0}},
               bucdate:parse("Monday 22 Aug 2008 6a", ?DATE)),
  ?assertEqual({{2008,8,22}, {18,35,0}},
               bucdate:parse("Mon, 22 Aug 2008 6:35 PM", ?DATE)).

t_parse_with_TZ() ->
  ?assertEqual({{2008,8,22}, {17,16,17}},
               bucdate:parse("Sat 22nd of August 2008 GMT", ?DATE)),
  ?assertEqual({{2008,8,22}, {17,16,17}},
               bucdate:parse("Sat 22nd of August 2008 UTC", ?DATE)),
  ?assertEqual({{2008,8,22}, {17,16,17}},
               bucdate:parse("Sat 22nd of August 2008 DST", ?DATE)).

t_iso() ->
  ?assertEqual("2004 W53",bucdate:format(?ISO,{{2005,1,1}, {1,1,1}})),
  ?assertEqual("2004 W53",bucdate:format(?ISO,{{2005,1,2}, {1,1,1}})),
  ?assertEqual("2005 W52",bucdate:format(?ISO,{{2005,12,31},{1,1,1}})),
  ?assertEqual("2007 W01",bucdate:format(?ISO,{{2007,1,1}, {1,1,1}})),
  ?assertEqual("2007 W52",bucdate:format(?ISO,{{2007,12,30},{1,1,1}})),
  ?assertEqual("2008 W01",bucdate:format(?ISO,{{2007,12,31},{1,1,1}})),
  ?assertEqual("2008 W01",bucdate:format(?ISO,{{2008,1,1}, {1,1,1}})),
  ?assertEqual("2009 W01",bucdate:format(?ISO,{{2008,12,29},{1,1,1}})),
  ?assertEqual("2009 W01",bucdate:format(?ISO,{{2008,12,31},{1,1,1}})),
  ?assertEqual("2009 W01",bucdate:format(?ISO,{{2009,1,1}, {1,1,1}})),
  ?assertEqual("2009 W53",bucdate:format(?ISO,{{2009,12,31},{1,1,1}})),
  ?assertEqual("2009 W53",bucdate:format(?ISO,{{2010,1,3}, {1,1,1}})).

t_ms() ->
  ?assertEqual({{2012,12,12}, {12,12,12,1234}}, bucdate:parse("2012-12-12T12:12:12.1234")),
  ?assertEqual(bucdate:format("H:m:s.f \\m \\i\\s \\m\\o\\n\\t\\h",?DATEMS),
               "17:03:17.123456 m is month"),
  ?assertEqual(bucdate:format("Y-m-d\\TH:i:s.f",?DATEMS),
               "2001-03-10T17:16:17.123456"),
  ?assertEqual(bucdate:format("Y-m-d\\TH:i:s.f",bucdate:nparse("2001-03-10T05:16:17.123456")),
               "2001-03-10T05:16:17.123456"),
  ?assertEqual(bucdate:format("Y-m-d\\TH:i:s.f",bucdate:nparse("2001-03-10T05:16:17.123456")),
               "2001-03-10T05:16:17.123456"),
  ?assertEqual(bucdate:format("Y-m-d\\TH:i:s.f",bucdate:nparse("2001-03-10T15:16:17.123456")),
               "2001-03-10T15:16:17.123456").

t_zulu() ->
  ?assertEqual(bucdate:format("Y-m-d\\TH:i:sZ",bucdate:nparse("2001-03-10T15:16:17.123456")),
               "2001-03-10T15:16:17Z"),
  ?assertEqual(bucdate:format("Y-m-d\\TH:i:s",bucdate:nparse("2001-03-10T15:16:17Z")),
               "2001-03-10T15:16:17"),
  ?assertEqual(bucdate:format("Y-m-d\\TH:i:s",bucdate:nparse("2001-03-10T15:16:17+04")),
               "2001-03-10T11:16:17"),
  ?assertEqual(bucdate:format("Y-m-d\\TH:i:s",bucdate:nparse("2001-03-10T15:16:17+04:00")),
               "2001-03-10T11:16:17"),
  ?assertEqual(bucdate:format("Y-m-d\\TH:i:s",bucdate:nparse("2001-03-10T15:16:17-04")),
               "2001-03-10T19:16:17"),
  ?assertEqual(bucdate:format("Y-m-d\\TH:i:s",bucdate:nparse("2001-03-10T15:16:17-04:00")),
               "2001-03-10T19:16:17").

t_timezone() ->
       meck:new(bucdate, [passthrough]),
       meck:expect(bucdate, today, 0, {date, {12, 0, 0}}),
       meck:expect(bucdate, today_utc, 0, {date, {10, 0, 0}}),
       ?assertEqual("+02:00", bucdate:local_timezone()),
       ?assertEqual(120, bucdate:timezone_offset()),

       meck:expect(bucdate, today, 0, {date, {10, 0, 0}}),
       meck:expect(bucdate, today_utc, 0, {date, {12, 0, 0}}),
       ?assertEqual("-02:00", bucdate:local_timezone()),
       ?assertEqual(-120, bucdate:timezone_offset()),

       meck:expect(bucdate, today, 0, {date, {11, 30, 0}}),
       meck:expect(bucdate, today_utc, 0, {date, {10, 0, 0}}),
       ?assertEqual("+01:30", bucdate:local_timezone()),
       ?assertEqual(90, bucdate:timezone_offset()),

       meck:expect(bucdate, today, 0, {date, {10, 30, 0}}),
       meck:expect(bucdate, today_utc, 0, {date, {12, 0, 0}}),
       ?assertEqual("-01:30", bucdate:local_timezone()),
       ?assertEqual(-90, bucdate:timezone_offset()),

       meck:unload(bucdate).

