%% @copyright Dale Harvey
%% @doc Format dates in erlang
%%
%% Licensed under the MIT license
%%
%% This module formats erlang dates in the form {{Year, Month, Day},
%% {Hour, Minute, Second}} to printable strings, using (almost)
%% equivalent formatting rules as http://uk.php.net/date, US vs
%% European dates are disambiguated in the same way as
%% http://uk.php.net/manual/en/function.strtotime.php That is, Dates
%% in the m/d/y or d-m-y formats are disambiguated by looking at the
%% separator between the various components: if the separator is a
%% slash (/), then the American m/d/y is assumed; whereas if the
%% separator is a dash (-) or a dot (.), then the European d-m-y
%% format is assumed. To avoid potential ambiguity, it's best to use
%% ISO 8601 (YYYY-MM-DD) dates.
%%
%% erlang has no concept of timezone so the following
%% formats are not implemented: B e I O P T Z
%% formats c and r will also differ slightly
%%
%% See tests at bottom for examples
-module(bucdate).
-author("Dale Harvey <dale@hypernumbers.com>").
-author("Gregoire Lejeune <gregoire.lejeune@gmail.com>").

-export([to_iso8601/1]).
-export([add/2, add/3]).
-export([today/0, yesterday/0, tomorrow/0]).
-export([today_utc/0, yesterday_utc/0, tomorrow_utc/0]).
-export([compare/2]).
-export([format/1, format/2]).
-export([parse/1, parse/2]).
-export([nparse/1]).
-export([local_timezone/0, timezone_offset/0]).

%% These are used exclusively as guards and so the function like
%% defines make sense
-define(IS_NUM(X), (X >= $0 andalso X =< $9)).
-define(IS_MERIDIAN(X), (X==[] orelse X==[am] orelse X==[pm])).
-define(IS_US_SEP(X), ( X==$/)).
-define(IS_WORLD_SEP(X), ( X==$-)).

-define(MONTH_TAG, month).
-define(IS_YEAR(X), (is_integer(X) andalso X > 31)).
-define(IS_DAY(X), (is_integer(X) andalso X =< 31)).
-define(IS_HINTED_MONTH(X), (is_tuple(X) andalso size(X)=:=2 andalso element(1, X)=:=?MONTH_TAG)).
-define(IS_MONTH(X), ((is_integer(X) andalso X =< 12) orelse ?IS_HINTED_MONTH(X))).

-define(GREGORIAN_SECONDS_1970, 62167219200).

-type daynum() :: 1..7.
-type now() :: {integer(), integer(), integer()}.
-type dt_units() :: seconds | minutes | hours | days | months | years.
-type dt_unit() :: second | minute | hour | day | month | year.

%% @doc
%% Return the local timezone
%% @end
local_timezone() ->
  Offset = timezone_offset(),
  M = abs(Offset) rem 60,
  H = bucs:to_integer((abs(Offset) - M) / 60),
  if
    Offset > 0 ->
      lists:flatten(io_lib:format("+~2..0w:~2..0w", [H, M]));
    true ->
      lists:flatten(io_lib:format("-~2..0w:~2..0w", [H, M]))
  end.

%% @doc
%% Returns the time difference between UTC time and local time, in minutes.
%% @end
timezone_offset() ->
  {{_, {LH, LM, LS}}, {_, {GH, GM, GS}}} = {bucdate:today(), bucdate:today_utc()},
  if
    LS =/= GS ->
      timezone_offset();
    true ->
      (LH - GH) * 60 + (LM - GM)
  end.

%% @doc
%% Add <tt>N</tt> units to the given <tt>DateTime</tt>.
%% @end
-spec add(calendar:datetime(), integer(), dt_units()) -> calendar:datetime().
add(DateTime, N, seconds) ->
    T1 = calendar:datetime_to_gregorian_seconds(DateTime),
    T2 = T1 + N,
    calendar:gregorian_seconds_to_datetime(T2);
add(DateTime, N, minutes) ->
    add(DateTime, 60*N, seconds);
add(DateTime, N, hours) ->
    add(DateTime, 60*N, minutes);
add(DateTime, N, days) ->
    add(DateTime, 24*N, hours);
add(DateTime, N, weeks) ->
    add(DateTime, 7*N, days);
add({{YYYY, MM, DD}=Date, Time}, 0, months) ->
    case calendar:valid_date(Date) of
        true  -> {Date, Time};
        false -> add({{YYYY, MM, DD-1}, Time}, 0, months)
    end;
add({{YYYY, MM, DD}, Time}, N, months) when N > 0 andalso MM < 12 ->
    add({{YYYY, MM+1, DD}, Time}, N-1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N > 0 andalso MM =:= 12 ->
    add({{YYYY+1, 1, DD}, Time}, N-1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N < 0 andalso MM > 1 ->
    add({{YYYY, MM-1, DD}, Time}, N+1, months);
add({{YYYY, MM, DD}, Time}, N, months) when N < 0 andalso MM =:= 1 ->
    add({{YYYY-1, 12, DD}, Time}, N+1, months);
add(Date, N, years) ->
    add(Date, 12*N, months).

%% @doc
%% Add 1 unit to the given <tt>DateTime</tt>.
%% @end
-spec add(calendar:datetime(), dt_unit()) -> calendar:datetime().
add(Date, second) ->
    add(Date, 1, seconds);
add(Date, minute) ->
    add(Date, 1, minutes);
add(Date, hour) ->
    add(Date, 1, hours);
add(Date, day) ->
    add(Date, 1);
add(Date, week) ->
    add(Date, 1, weeks);
add(Date, month) ->
    add(Date, 1, months);
add(Date, year) ->
    add(Date, 1, years);
add(Date, N)  ->
    add(Date, N, days).

% @doc
% return now local datetime.
% @end
-spec today() -> calendar:datetime().
today() -> erlang:localtime().

% @doc
% return tomorrow local datetime.
% @end
-spec tomorrow() -> calendar:datetime().
tomorrow() -> add(today(), 1).

% @doc
% return yesterday local datetime.
% @end
-spec yesterday() -> calendar:datetime().
yesterday() -> add(today(), -1).

% @doc
% return now UTC datetime.
% @end
-spec today_utc() -> calendar:datetime().
today_utc() -> erlang:universaltime().

% @doc
% return tomorrow UTC datetime.
% @end
-spec tomorrow_utc() -> calendar:datetime().
tomorrow_utc() -> add(today_utc(), 1).

% @doc
% return yesterday UTC datetime.
% @end
-spec yesterday_utc() -> calendar:datetime().
yesterday_utc() -> add(today_utc(), -1).

% @doc
% Compate <tt>Date1</tt> and <tt>Date2</tt>
% @end
-spec compare(calendar:datetime(), calendar:datetime()) -> -1 | 1 | 0.
compare(Date1, Date2) ->
    SecDate1 = calendar:datetime_to_gregorian_seconds(Date1),
    SecDate2 = calendar:datetime_to_gregorian_seconds(Date2),
    if
        SecDate1 > SecDate2 -> -1;
        SecDate2 > SecDate1 -> 1;
        true -> 0
    end.

%% @doc
%% Return date using iso8601 format
%% @end
-spec to_iso8601(calendar:datetime()) -> string().
to_iso8601(Date) ->
    format("Y-m-dTH:i:s", Date) ++ local_timezone().

%% @doc format current local time as Format
-spec format(string()) -> string().
format(Format) ->
    format(Format, calendar:universal_time(), []).

%% @doc format Date as Format
-spec format(string(), calendar:datetime() | now()) -> string().
format(Format, {_, _, Ms}=Now) ->
    {Date, {H, M, S}} = calendar:now_to_datetime(Now),
    format(Format, {Date, {H, M, S, Ms}}, []);
format(Format, Date) ->
    format(Format, Date, []).

%% @doc parses the datetime from a string
-spec parse(string()) -> calendar:datetime().
parse(Date) ->
    do_parse(Date, calendar:universal_time(), []).

%% @doc parses the datetime from a string
-spec parse(string(), calendar:datetime() | now()) -> calendar:datetime().
parse(Date, {_, _, _}=Now) ->
    do_parse(Date, calendar:now_to_datetime(Now), []);
parse(Date, Now) ->
    do_parse(Date, Now, []).

do_parse(Date, Now, Opts) ->
    case filter_hints(parse(tokenise(string:to_upper(Date), []), Now, Opts)) of
        {error, bad_date} ->
            erlang:throw({?MODULE, {bad_date, Date}});
        {D1, T1} = {{Y, M, D}, {H, M1, S}}
          when is_number(Y), is_number(M),
               is_number(D), is_number(H),
               is_number(M1), is_number(S) ->
            case calendar:valid_date(D1) of
                true -> {D1, T1};
                false -> erlang:throw({?MODULE, {bad_date, Date}})
            end;
        {D1, _T1, {Ms}} = {{Y, M, D}, {H, M1, S},  {Ms}}
          when is_number(Y), is_number(M),
               is_number(D), is_number(H),
               is_number(M1), is_number(S),
               is_number(Ms) ->
            case calendar:valid_date(D1) of
                true -> {D1, {H, M1, S, Ms}};
                false -> erlang:throw({?MODULE, {bad_date, Date}})
            end;
        Unknown -> erlang:throw({?MODULE, {bad_date, Date, Unknown }})
    end.

filter_hints({{Y, {?MONTH_TAG, M}, D}, {H, M1, S}}) ->
    filter_hints({{Y, M, D}, {H, M1, S}});
filter_hints({{Y, {?MONTH_TAG, M}, D}, {H, M1, S}, {Ms}}) ->
    filter_hints({{Y, M, D}, {H, M1, S}, {Ms}});
filter_hints(Other) ->
    Other.

-spec nparse(string()) -> now().
%% @doc parses the datetime from a string into 'now' format
nparse(Date) ->
    case parse(Date) of
        {DateS, {H, M, S, Ms} } ->
            GSeconds = calendar:datetime_to_gregorian_seconds({DateS, {H, M, S} }),
            ESeconds = GSeconds - ?GREGORIAN_SECONDS_1970,
            {ESeconds div 1000000, ESeconds rem 1000000, Ms};
        DateTime ->
            GSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
            ESeconds = GSeconds - ?GREGORIAN_SECONDS_1970,
            {ESeconds div 1000000, ESeconds rem 1000000, 0}
    end.

%%
%% LOCAL FUNCTIONS
%%

parse([Year, X, Month, X, Day, Hour, $:, Min, $:, Sec, $Z ], _Now, _Opts)
  when  (?IS_US_SEP(X) orelse ?IS_WORLD_SEP(X))
        andalso Year > 31 ->
    {{Year, Month, Day}, {hour(Hour, []), Min, Sec}, { 0}};

parse([Year, X, Month, X, Day, Hour, $:, Min, $:, Sec, $+, Off | _Rest ], _Now, _Opts)
  when  (?IS_US_SEP(X) orelse ?IS_WORLD_SEP(X))
        andalso Year > 31 ->
    {{Year, Month, Day}, {hour(Hour, []) - Off, Min, Sec}, {0}};

parse([Year, X, Month, X, Day, Hour, $:, Min, $:, Sec, $-, Off | _Rest ], _Now, _Opts)
  when  (?IS_US_SEP(X) orelse ?IS_WORLD_SEP(X))
        andalso Year > 31 ->
    {{Year, Month, Day}, {hour(Hour, []) + Off, Min, Sec}, {0}};

%% Date/Times 22 Aug 2008 6:35.0001 PM
parse([Year, X, Month, X, Day, Hour, $:, Min, $:, Sec, $., Ms | PAM], _Now, _Opts)
  when ?IS_MERIDIAN(PAM) andalso
       (?IS_US_SEP(X) orelse ?IS_WORLD_SEP(X))
       andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}, {Ms}};
parse([Year, X, Month, X, Day, Hour, $:, Min, $:, Sec, $., Ms , $Z], _Now, _Opts)
  when (?IS_US_SEP(X) orelse ?IS_WORLD_SEP(X)) andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, {hour(Hour, []), Min, Sec}, {Ms}};
parse([Month, X, Day, X, Year, Hour, $:, Min, $:, Sec, $., Ms | PAM], _Now, _Opts)
  when ?IS_MERIDIAN(PAM) andalso ?IS_US_SEP(X)
       andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}, {Ms}};
parse([Day, X, Month, X, Year, Hour, $:, Min, $:, Sec, $., Ms | PAM], _Now, _Opts)
  when ?IS_MERIDIAN(PAM) andalso ?IS_WORLD_SEP(X)
       andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}, {Ms}};

parse([Year, X, Month, X, Day, Hour, $:, Min, $:, Sec, $., Ms], _Now, _Opts)
  when  (?IS_US_SEP(X) orelse ?IS_WORLD_SEP(X))
        andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, {hour(Hour, []), Min, Sec}, {Ms}};
parse([Month, X, Day, X, Year, Hour, $:, Min, $:, Sec, $., Ms], _Now, _Opts)
  when ?IS_US_SEP(X) andalso ?IS_MONTH(Month) ->
    {{Year, Month, Day}, {hour(Hour, []), Min, Sec}, {Ms}};
parse([Day, X, Month, X, Year, Hour, $:, Min, $:, Sec, $., Ms ], _Now, _Opts)
  when ?IS_WORLD_SEP(X) andalso ?IS_MONTH(Month) ->
    {{Year, Month, Day}, {hour(Hour, []), Min, Sec}, {Ms}};

%% Date/Times Dec 1st, 2012 6:25 PM
parse([Month, Day, Year, Hour, $:, Min, $:, Sec | PAM], _Now, _Opts)
  when ?IS_MERIDIAN(PAM) andalso ?IS_HINTED_MONTH(Month) andalso ?IS_DAY(Day) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}};
parse([Month, Day, Year, Hour, $:, Min | PAM], _Now, _Opts)
  when ?IS_MERIDIAN(PAM) andalso ?IS_HINTED_MONTH(Month) andalso ?IS_DAY(Day) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, 0}};
parse([Month, Day, Year, Hour | PAM], _Now, _Opts)
  when ?IS_MERIDIAN(PAM) andalso ?IS_HINTED_MONTH(Month) andalso ?IS_DAY(Day) ->
    {{Year, Month, Day}, {hour(Hour, PAM), 0, 0}};

%% Date/Times Dec 1st, 2012 18:25:15 (no AM/PM)
parse([Month, Day, Year, Hour, $:, Min, $:, Sec], _Now, _Opts)
  when ?IS_HINTED_MONTH(Month) andalso ?IS_DAY(Day) ->
    {{Year, Month, Day}, {hour(Hour, []), Min, Sec}};
parse([Month, Day, Year, Hour, $:, Min], _Now, _Opts)
  when ?IS_HINTED_MONTH(Month) andalso ?IS_DAY(Day) ->
    {{Year, Month, Day}, {hour(Hour, []), Min, 0}};

%% Times - 21:45, 13:45:54, 13:15PM etc
parse([Hour, $:, Min, $:, Sec | PAM], {Date, _Time}, _O) when ?IS_MERIDIAN(PAM) ->
    {Date, {hour(Hour, PAM), Min, Sec}};
parse([Hour, $:, Min | PAM], {Date, _Time}, _Opts) when ?IS_MERIDIAN(PAM) ->
    {Date, {hour(Hour, PAM), Min, 0}};
parse([Hour | PAM], {Date, _Time}, _Opts) when ?IS_MERIDIAN(PAM) ->
    {Date, {hour(Hour, PAM), 0, 0}};

%% Dates (Any combination with word month "aug 8th, 2008", "8 aug 2008", "2008 aug 21" "2008 5 aug" )
%% Will work because of the "Hinted month"
parse([Day, Month, Year], {_Date, Time}, _Opts)
  when ?IS_DAY(Day) andalso ?IS_HINTED_MONTH(Month) andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, Time};
parse([Month, Day, Year], {_Date, Time}, _Opts)
  when ?IS_DAY(Day) andalso ?IS_HINTED_MONTH(Month) andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, Time};
parse([Year, Day, Month], {_Date, Time}, _Opts)
  when ?IS_DAY(Day) andalso ?IS_HINTED_MONTH(Month) andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, Time};
parse([Year, Month, Day], {_Date, Time}, _Opts)
  when ?IS_DAY(Day) andalso ?IS_HINTED_MONTH(Month) andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, Time};

%% Dates 23/april/1963
parse([Day, Month, Year], {_Date, Time}, _Opts) ->
    {{Year, Month, Day}, Time};
parse([Year, X, Month, X, Day], {_Date, Time}, _Opts)
  when (?IS_US_SEP(X) orelse ?IS_WORLD_SEP(X))
       andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, Time};
parse([Month, X, Day, X, Year], {_Date, Time}, _Opts) when ?IS_US_SEP(X) ->
    {{Year, Month, Day}, Time};
parse([Day, X, Month, X, Year], {_Date, Time}, _Opts) when ?IS_WORLD_SEP(X) ->
    {{Year, Month, Day}, Time};

%% Date/Times 22 Aug 2008 6:35 PM
%% Time is "7 PM"
parse([Year, X, Month, X, Day, Hour | PAM], _Date, _Opts)
  when ?IS_MERIDIAN(PAM) andalso
       (?IS_US_SEP(X) orelse ?IS_WORLD_SEP(X))
       andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, {hour(Hour, PAM), 0, 0}};
parse([Day, X, Month, X, Year, Hour | PAM], _Date, _Opts)
  when ?IS_MERIDIAN(PAM) andalso ?IS_WORLD_SEP(X) ->
    {{Year, Month, Day}, {hour(Hour, PAM), 0, 0}};
parse([Month, X, Day, X, Year, Hour | PAM], _Date, _Opts)
  when ?IS_MERIDIAN(PAM) andalso ?IS_US_SEP(X) ->
    {{Year, Month, Day}, {hour(Hour, PAM), 0, 0}};


%% Time is "6:35 PM" ms return
parse([Year, X, Month, X, Day, Hour, $:, Min | PAM], _Date, _Opts)
  when ?IS_MERIDIAN(PAM) andalso
       (?IS_US_SEP(X) orelse ?IS_WORLD_SEP(X))
       andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, 0}};
parse([Day, X, Month, X, Year, Hour, $:, Min | PAM], _Date, _Opts)
  when ?IS_MERIDIAN(PAM) andalso ?IS_WORLD_SEP(X) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, 0}};
parse([Month, X, Day, X, Year, Hour, $:, Min | PAM], _Date, _Opts)
  when ?IS_MERIDIAN(PAM) andalso ?IS_US_SEP(X) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, 0}};

%% Time is "6:35:15 PM"
parse([Year, X, Month, X, Day, Hour, $:, Min, $:, Sec | PAM], _Now, _Opts)
  when ?IS_MERIDIAN(PAM) andalso
       (?IS_US_SEP(X) orelse ?IS_WORLD_SEP(X))
       andalso ?IS_YEAR(Year) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}};
parse([Month, X, Day, X, Year, Hour, $:, Min, $:, Sec | PAM], _Now, _Opts)
  when ?IS_MERIDIAN(PAM) andalso ?IS_US_SEP(X) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}};
parse([Day, X, Month, X, Year, Hour, $:, Min, $:, Sec | PAM], _Now, _Opts)
  when ?IS_MERIDIAN(PAM) andalso ?IS_WORLD_SEP(X) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}};

parse([Day, Month, Year, Hour | PAM], _Now, _Opts)
  when ?IS_MERIDIAN(PAM) ->
    {{Year, Month, Day}, {hour(Hour, PAM), 0, 0}};
parse([Day, Month, Year, Hour, $:, Min | PAM], _Now, _Opts)
  when ?IS_MERIDIAN(PAM) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, 0}};
parse([Day, Month, Year, Hour, $:, Min, $:, Sec | PAM], _Now, _Opts)
  when ?IS_MERIDIAN(PAM) ->
    {{Year, Month, Day}, {hour(Hour, PAM), Min, Sec}};

parse(_Tokens, _Now, _Opts) ->
    {error, bad_date}.

tokenise([], Acc) ->
    lists:reverse(Acc);

tokenise([N1, N2, N3, N4, N5, N6 | Rest], Acc)
  when ?IS_NUM(N1), ?IS_NUM(N2), ?IS_NUM(N3), ?IS_NUM(N4), ?IS_NUM(N5), ?IS_NUM(N6) ->
    tokenise(Rest, [ ltoi([N1, N2, N3, N4, N5, N6]) | Acc]);
tokenise([N1, N2, N3, N4, N5 | Rest], Acc)
  when ?IS_NUM(N1), ?IS_NUM(N2), ?IS_NUM(N3), ?IS_NUM(N4), ?IS_NUM(N5) ->
    tokenise(Rest, [ ltoi([N1, N2, N3, N4, N5]) | Acc]);
tokenise([N1, N2, N3, N4 | Rest], Acc)
  when ?IS_NUM(N1), ?IS_NUM(N2), ?IS_NUM(N3), ?IS_NUM(N4) ->
    tokenise(Rest, [ ltoi([N1, N2, N3, N4]) | Acc]);
tokenise([N1, N2, N3 | Rest], Acc)
  when ?IS_NUM(N1), ?IS_NUM(N2), ?IS_NUM(N3) ->
    tokenise(Rest, [ ltoi([N1, N2, N3]) | Acc]);
tokenise([N1, N2 | Rest], Acc)
  when ?IS_NUM(N1), ?IS_NUM(N2) ->
    tokenise(Rest, [ ltoi([N1, N2]) | Acc]);
tokenise([N1 | Rest], Acc)
  when ?IS_NUM(N1) ->
    tokenise(Rest, [ ltoi([N1]) | Acc]);


%% Worded Months get tagged with ?MONTH_TAG to let the parser know that these
%% are unambiguously declared to be months. This was there's no confusion
%% between, for example: "Aug 12" and "12 Aug"
%% These hint tags are filtered in filter_hints/1 above.
tokenise("JANUARY" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 1} | Acc]);
tokenise("JAN" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 1} | Acc]);
tokenise("FEBRUARY" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 2} | Acc]);
tokenise("FEB" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 2} | Acc]);
tokenise("MARCH" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 3} | Acc]);
tokenise("MAR" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 3} | Acc]);
tokenise("APRIL" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 4} | Acc]);
tokenise("APR" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 4} | Acc]);
tokenise("MAY" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 5} | Acc]);
tokenise("JUNE" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 6} | Acc]);
tokenise("JUN" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 6} | Acc]);
tokenise("JULY" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 7} | Acc]);
tokenise("JUL" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 7} | Acc]);
tokenise("AUGUST" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 8} | Acc]);
tokenise("AUG" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 8} | Acc]);
tokenise("SEPTEMBER" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 9} | Acc]);
tokenise("SEPT" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 9} | Acc]);
tokenise("SEP" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 9} | Acc]);
tokenise("OCTOBER" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 10} | Acc]);
tokenise("OCT" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 10} | Acc]);
tokenise("NOVEMBER" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 11} | Acc]);
tokenise("NOVEM" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 11} | Acc]);
tokenise("NOV" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 11} | Acc]);
tokenise("DECEMBER" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 12} | Acc]);
tokenise("DECEM" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 12} | Acc]);
tokenise("DEC" ++ Rest, Acc) -> tokenise(Rest, [{?MONTH_TAG, 12} | Acc]);

tokenise([$: | Rest], Acc) -> tokenise(Rest, [ $: | Acc]);
tokenise([$/ | Rest], Acc) -> tokenise(Rest, [ $/ | Acc]);
tokenise([$- | Rest], Acc) -> tokenise(Rest, [ $- | Acc]);
tokenise("AM" ++ Rest, Acc) -> tokenise(Rest, [am | Acc]);
tokenise("PM" ++ Rest, Acc) -> tokenise(Rest, [pm | Acc]);
tokenise("A" ++ Rest, Acc) -> tokenise(Rest, [am | Acc]);
tokenise("P" ++ Rest, Acc) -> tokenise(Rest, [pm | Acc]);

%% Postel's Law
%%
%% be conservative in what you do,
%% be liberal in what you accept from others.
%%
%% See RFC 793 Section 2.10 http://tools.ietf.org/html/rfc793
%%
%% Mebbies folk want to include Saturday etc in a date, nae borra
tokenise("MONDAY" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("MON" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("TUESDAY" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("TUES" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("TUE" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("WEDNESDAY" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("WEDS" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("WED" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("THURSDAY" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("THURS" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("THUR" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("THU" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("FRIDAY" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("FRI" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("SATURDAY" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("SAT" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("SUNDAY" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("SUN" ++ Rest, Acc) -> tokenise(Rest, Acc);

%% Hmm Excel reports GMT in times so nuke that too
tokenise("GMT" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("UTC" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("DST" ++ Rest, Acc) -> tokenise(Rest, Acc); % daylight saving time

tokenise([$, | Rest], Acc) -> tokenise(Rest, Acc);
tokenise([32 | Rest], Acc) -> tokenise(Rest, Acc); % Spaces
tokenise("TH" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("ND" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("ST" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("OF" ++ Rest, Acc) -> tokenise(Rest, Acc);
tokenise("T" ++ Rest, Acc) -> tokenise(Rest, Acc);  % 2012-12-12T12:12:12 ISO formatting.
tokenise([$Z | Rest], Acc) -> tokenise(Rest, [$Z | Acc]);  % 2012-12-12T12:12:12Zulu
tokenise([$. |  Rest], Acc) -> tokenise(Rest, [$. | Acc]);  % 2012-12-12T12:12:12.xxxx ISO formatting.
tokenise([$+| Rest], Acc) -> tokenise(Rest, [$+ | Acc]);  % 2012-12-12T12:12:12.xxxx+ ISO formatting.

tokenise([Else | Rest], Acc) ->
    tokenise(Rest, [{bad_token, Else} | Acc]).

hour(Hour, []) -> Hour;
hour(12, [am]) -> 0;
hour(Hour, [am]) -> Hour;
hour(12, [pm]) -> 12;
hour(Hour, [pm]) -> Hour+12.

%% Finished, return
format([], _Date, Acc) ->
    lists:flatten(lists:reverse(Acc));

%% Escape backslashes
format([$\\, H|T], Dt, Acc) ->
    format(T, Dt, [H|Acc]);

%% Year Formats
format([$Y|T], {{Y, _, _}, _}=Dt, Acc) ->
    format(T, Dt, [itol(Y)|Acc]);
format([$y|T], {{Y, _, _}, _}=Dt, Acc) ->
    [_, _, Y3, Y4] = itol(Y),
    format(T, Dt, [[Y3, Y4]|Acc]);
format([$L|T], {{Y, _, _}, _}=Dt, Acc) ->
    format(T, Dt, [itol(is_leap(Y))|Acc]);
format([$o|T], {Date, _}=Dt, Acc) ->
    format(T, Dt, [itol(iso_year(Date))|Acc]);

%% Month Formats
format([$n|T], {{_, M, _}, _}=Dt, Acc) ->
    format(T, Dt, [itol(M)|Acc]);
format([$m|T], {{_, M, _}, _}=Dt, Acc) ->
    format(T, Dt, [pad2(M)|Acc]);
format([$M|T], {{_, M, _}, _}=Dt, Acc) ->
    format(T, Dt, [smonth(M)|Acc]);
format([$F|T], {{_, M, _}, _}=Dt, Acc) ->
    format(T, Dt, [month(M)|Acc]);
format([$t|T], {{Y, M, _}, _}=Dt, Acc) ->
    format(T, Dt, [itol(calendar:last_day_of_the_month(Y, M))|Acc]);

%% Week Formats
format([$W|T], {Date, _}=Dt, Acc) ->
    format(T, Dt, [pad2(iso_week(Date))|Acc]);

%% Day Formats
format([$j|T], {{_, _, D}, _}=Dt, Acc) ->
    format(T, Dt, [itol(D)|Acc]);
format([$S|T], {{_, _, D}, _}=Dt, Acc) ->
    format(T, Dt, [suffix(D)| Acc]);
format([$d|T], {{_, _, D}, _}=Dt, Acc) ->
    format(T, Dt, [pad2(D)|Acc]);
format([$D|T], {Date, _}=Dt, Acc) ->
    format(T, Dt, [sdayd(Date)|Acc]);
format([$l|T], {Date, _}=Dt, Acc) ->
    format(T, Dt, [day(calendar:day_of_the_week(Date))|Acc]);
format([$N|T], {Date, _}=Dt, Acc) ->
    format(T, Dt, [itol(calendar:day_of_the_week(Date))|Acc]);
format([$w|T], {Date, _}=Dt, Acc) ->
    format(T, Dt, [itol(to_w(calendar:day_of_the_week(Date)))|Acc]);
format([$z|T], {Date, _}=Dt, Acc) ->
    format(T, Dt, [itol(days_in_year(Date))|Acc]);

%% Time Formats
format([$a|T], Dt={_, {H, _, _}}, Acc) when H >= 12 ->
    format(T, Dt, ["pm"|Acc]);
format([$a|T], Dt={_, {_, _, _}}, Acc) ->
    format(T, Dt, ["am"|Acc]);
format([$A|T], {_, {H, _, _}}=Dt, Acc) when H >= 12 ->
    format(T, Dt, ["PM"|Acc]);
format([$A|T], Dt={_, {_, _, _}}, Acc) ->
    format(T, Dt, ["AM"|Acc]);
format([$g|T], {_, {H, _, _}}=Dt, Acc) when H == 12; H == 0 ->
    format(T, Dt, ["12"|Acc]);
format([$g|T], {_, {H, _, _}}=Dt, Acc) when H > 12 ->
    format(T, Dt, [itol(H-12)|Acc]);
format([$g|T], {_, {H, _, _}}=Dt, Acc) ->
    format(T, Dt, [itol(H)|Acc]);
format([$G|T], {_, {H, _, _}}=Dt, Acc) ->
    format(T, Dt, [itol(H)|Acc]);
format([$h|T], {_, {H, _, _}}=Dt, Acc) when H > 12 ->
    format(T, Dt, [pad2(H-12)|Acc]);
format([$h|T], {_, {H, _, _}}=Dt, Acc) ->
    format(T, Dt, [pad2(H)|Acc]);
format([$H|T], {_, {H, _, _}}=Dt, Acc) ->
    format(T, Dt, [pad2(H)|Acc]);
format([$i|T], {_, {_, M, _}}=Dt, Acc) ->
    format(T, Dt, [pad2(M)|Acc]);
format([$s|T], {_, {_, _, S}}=Dt, Acc) ->
    format(T, Dt, [pad2(S)|Acc]);
format([$f|T], {_, {_, _, _}}=Dt, Acc) ->
    format(T, Dt, [itol(0)|Acc]);

%% Time Formats ms
format([$a|T], Dt={_, {H, _, _, _}}, Acc) when H > 12 ->
    format(T, Dt, ["pm"|Acc]);
format([$a|T], Dt={_, {_, _, _, _}}, Acc) ->
    format(T, Dt, ["am"|Acc]);
format([$A|T], {_, {H, _, _, _}}=Dt, Acc) when H > 12 ->
    format(T, Dt, ["PM"|Acc]);
format([$A|T], Dt={_, {_, _, _, _}}, Acc) ->
    format(T, Dt, ["AM"|Acc]);
format([$g|T], {_, {H, _, _, _}}=Dt, Acc) when H == 12; H == 0 ->
    format(T, Dt, ["12"|Acc]);
format([$g|T], {_, {H, _, _, _}}=Dt, Acc) when H > 12 ->
    format(T, Dt, [itol(H-12)|Acc]);
format([$g|T], {_, {H, _, _, _}}=Dt, Acc) ->
    format(T, Dt, [itol(H)|Acc]);
format([$G|T], {_, {H, _, _, _}}=Dt, Acc) ->
    format(T, Dt, [itol(H)|Acc]);
format([$h|T], {_, {H, _, _, _}}=Dt, Acc) when H > 12 ->
    format(T, Dt, [pad2(H-12)|Acc]);
format([$h|T], {_, {H, _, _, _}}=Dt, Acc) ->
    format(T, Dt, [pad2(H)|Acc]);
format([$H|T], {_, {H, _, _, _}}=Dt, Acc) ->
    format(T, Dt, [pad2(H)|Acc]);
format([$i|T], {_, {_, M, _, _}}=Dt, Acc) ->
    format(T, Dt, [pad2(M)|Acc]);
format([$s|T], {_, {_, _, S, _}}=Dt, Acc) ->
    format(T, Dt, [pad2(S)|Acc]);
format([$f|T], {_, {_, _, _, Ms}}=Dt, Acc) ->
    format(T, Dt, [itol(Ms)|Acc]);

%% Whole Dates
format([$c|T], {{Y, M, D}, {H, Min, S}}=Dt, Acc) ->
    Format = "~4.10.0B-~2.10.0B-~2.10.0B"
    ++ " ~2.10.0B:~2.10.0B:~2.10.0B",
    Date = io_lib:format(Format, [Y, M, D, H, Min, S]),
    format(T, Dt, [Date|Acc]);
format([$r|T], {{Y, M, D}, {H, Min, S}}=Dt, Acc) ->
    Format = "~s, ~p ~s ~p ~2.10.0B:~2.10.0B:~2.10.0B",
    Args = [sdayd({Y, M, D}), D, smonth(M), Y, H, Min, S],
    format(T, Dt, [io_lib:format(Format, Args)|Acc]);
format([$U|T], Dt, Acc) ->
    Epoch = {{1970, 1, 1}, {0, 0, 0}},
    Time = calendar:datetime_to_gregorian_seconds(Dt) -
    calendar:datetime_to_gregorian_seconds(Epoch),
    format(T, Dt, [itol(Time)|Acc]);

%% Unrecognised, print as is
format([H|T], Date, Acc) ->
    format(T, Date, [H|Acc]).


days_in_year({Y, _, _}=Date) ->
    calendar:date_to_gregorian_days(Date) -
    calendar:date_to_gregorian_days({Y, 1, 1}).

is_leap(Y) ->
    case calendar:is_leap_year(Y) of
        true -> 1;
        false -> 0
    end.

%% @doc Made up numeric day of the week
%% (0 Sunday -> 6 Saturday)
-spec to_w(daynum()) -> integer().
to_w(7) -> 0;
to_w(X) -> X.

suffix(1) -> "st";
suffix(2) -> "nd";
suffix(3) -> "rd";
suffix(21) -> "st";
suffix(22) -> "nd";
suffix(23) -> "rd";
suffix(31) -> "st";
suffix(_) -> "th".

sdayd({Y, M, D}) ->
    sday(calendar:day_of_the_week({Y, M, D})).

sday(1) -> "Mon";
sday(2) -> "Tue";
sday(3) -> "Wed";
sday(4) -> "Thu";
sday(5) -> "Fri";
sday(6) -> "Sat";
sday(7) -> "Sun".

day(1) -> "Monday";
day(2) -> "Tuesday";
day(3) -> "Wednesday";
day(4) -> "Thursday";
day(5) -> "Friday";
day(6) -> "Saturday";
day(7) -> "Sunday".

smonth(1) -> "Jan";
smonth(2) -> "Feb";
smonth(3) -> "Mar";
smonth(4) -> "Apr";
smonth(5) -> "May";
smonth(6) -> "Jun";
smonth(7) -> "Jul";
smonth(8) -> "Aug";
smonth(9) -> "Sep";
smonth(10) -> "Oct";
smonth(11) -> "Nov";
smonth(12) -> "Dec".

month(1) -> "January";
month(2) -> "February";
month(3) -> "March";
month(4) -> "April";
month(5) -> "May";
month(6) -> "June";
month(7) -> "July";
month(8) -> "August";
month(9) -> "September";
month(10) -> "October";
month(11) -> "November";
month(12) -> "December".

iso_week(Date) ->
    Week = iso_week_one(iso_year(Date)),
    Days = calendar:date_to_gregorian_days(Date) -
    calendar:date_to_gregorian_days(Week),
    trunc((Days / 7) + 1).

iso_year({Y, _M, _D}=Dt) ->
    case Dt >= {Y, 12, 29} of
        true ->
            case Dt < iso_week_one(Y+1) of
                true -> Y;
                false -> Y+1
            end;
        false ->
            case Dt < iso_week_one(Y) of
                true -> Y-1;
                false -> Y
            end
    end.

iso_week_one(Y) ->
    Day1 = calendar:day_of_the_week({Y, 1, 4}),
    Days = calendar:date_to_gregorian_days({Y, 1, 4}) + (1-Day1),
    calendar:gregorian_days_to_date(Days).

itol(X) ->
    integer_to_list(X).

pad2(X) when is_integer(X) ->
    io_lib:format("~2.10.0B", [X]);
pad2(X) when is_float(X) ->
    io_lib:format("~2.10.0B", [trunc(X)]).

ltoi(X) ->
    list_to_integer(X).

