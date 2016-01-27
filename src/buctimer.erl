-module(buctimer).

-export([verify/1, next/1, next/2]).

-type period() :: atom().
-type day_of_week() :: monday | thursday | wednesday | thursday | friday | saturday | sunday.
-type year() :: calendar:year() | [calendar:year()] | period().
-type month() :: calendar:month() | [calendar:month()] | period().
-type day() :: calendar:day() | [calendar:day()] | period() | [day_of_week()].
-type hour() :: calendar:hour() | [calendar:hour()] | period().
-type minute() :: calendar:minute() | [calendar:minute()] | period().
-type second() :: calendar:second() | [calendar:second()] | period().
-type timer_spec() :: {year(), month(), day(), hour(), minute(), second()}.
-type datetime() :: {calendar:year(), calendar:month(), calendar:day(), calendar:hour(), calendar:minute(), calendar:second()}.

% @doc
% Verify the timer syntax.
% @end
-spec verify(timer_spec()) -> ok | {error, term()}.
verify({_, _, _, _, _, _} = Spec) ->
  Exp = expand(bucs:to_list(Spec)),
  Check = lists:append([[{U, N} || N <- L] ||
                        {U, L} <- lists:zip(units(), Exp)]),
  validate(Check).

% @doc
% Return the next datetime from now.
% @end
-spec next(timer_spec()) -> {ok, calendar:datetime(), calendar:second()} | {error, term()} | stop.
next(Spec) ->
  next(Spec, n()).

% @doc
% Return the next datetime from the given datetime.
% @end
-spec next(timer_spec(), calendar:datetime() | datetime()) -> {ok, calendar:datetime(), calendar:second()} | {error, term()} | stop.
next(Spec, {{Y, M, D}, {HH, MM, SS}}) ->
  next(Spec, {Y, M, D, HH, MM, SS});
next({_, _, _, _, _, _} = Spec,
     {DTY, DTM, DTD, DTHH, DTMM, DTSS} = From) ->
  case verify(Spec) of
    ok ->
      FromAsDT = {{DTY, DTM, DTD}, {DTHH, DTMM, DTSS}},
      Assoc = lists:zip3(units(), bucs:to_list(Spec), bucs:to_list(From)),
      case lists:keyfind(year, 1, Assoc) of
        {year, _, _} = X ->
          Years = find_next(X),
          case lists:keyfind(month, 1, Assoc) of
            {month, _, _} = X1 ->
              Months = find_next(X1),
              case lists:keyfind(day, 1, Assoc) of
                {day, _, _} = X2 ->
                  Days = find_next(X2),
                  case lists:keyfind(hour, 1, Assoc) of
                    {hour, _, _} = X3 ->
                      Hours= find_next(X3),
                      case lists:keyfind(minute, 1, Assoc) of
                        {minute, _, _} = X4 ->
                          Minutes = find_next(X4),
                          case lists:keyfind(second, 1, Assoc) of
                            {second, _, _} = X5 ->
                              Seconds = find_next(X5),
                              Dates = real_dates([{Y, M, D} || Y <- Years,
                                                               M <- Months,
                                                               D <- Days]),
                              Times = [{HH, MM, SS} || HH <- Hours,
                                                       MM <- Minutes,
                                                       SS <- Seconds],
                              MinGap = min_gap(Spec),
                              case lists:foldl(fun({D, _} = DateTime, Result) ->
                                                   case calendar:valid_date(D) of
                                                     true ->
                                                       get_date(MinGap, DateTime, FromAsDT, Result);
                                                     false ->
                                                       Result
                                                   end
                                               end,
                                               undefined,
                                               [{Date, Time} || Date <- Dates, Time <- Times]) of
                                undefined -> stop;
                                Result ->
                                  {ok,
                                   Result,
                                   calendar:datetime_to_gregorian_seconds(Result) -
                                   calendar:datetime_to_gregorian_seconds(FromAsDT)}
                              end;
                            _ -> {error, second}
                          end;
                        _ -> {error, minute}
                      end;
                    _ -> {error, hour}
                  end;
                _ -> {error, day}
              end;
            _ -> {error, month}
          end;
        _ -> {error, year}
      end;
    Reason -> Reason
  end.

n() ->
  {{Y, M, D}, {H, Mo, S}} = calendar:local_time(),
  {Y, M, D, H, Mo, S}.

expand(Spec) ->
  lists:map(fun
              (L) when is_list(L) -> lists:usort(L);
              (N) -> [N]
            end, Spec).

units() -> [year, month, day, hour, minute, second].

validate([]) -> ok;
validate([{year, Y}|Rest]) when is_integer(Y), Y > 0 -> validate(Rest);
validate([{month, M}|Rest]) when is_integer(M), M >= 1, M =< 12 -> validate(Rest);
validate([{day, D}|Rest]) when is_integer(D), D >= 1, D =< 31 -> validate(Rest);
validate([{day, monday}|Rest]) -> validate(Rest);
validate([{day, tuesday}|Rest]) -> validate(Rest);
validate([{day, wednesday}|Rest]) -> validate(Rest);
validate([{day, thursday}|Rest]) -> validate(Rest);
validate([{day, friday}|Rest]) -> validate(Rest);
validate([{day, saturday}|Rest]) -> validate(Rest);
validate([{day, sunday}|Rest]) -> validate(Rest);
validate([{hour, H}|Rest]) when is_integer(H), H >= 0,  H =< 23 -> validate(Rest);
validate([{minute, M}|Rest]) when is_integer(M), M >= 0,  M =< 59 -> validate(Rest);
validate([{second, S}|Rest]) when is_integer(S), S >= 0,  S =< 59 -> validate(Rest);
validate([{Type, Value}|Rest]) when is_atom(Value) ->
  case binary:split(bucs:to_binary(Value), <<"/">>) of
    [<<"*">>] -> validate(Rest);
    [<<"*">>, Data] ->
      try bucs:to_integer(Data) of
        _ -> validate(Rest)
      catch
        _:_ -> {error, Type}
      end;
    _ -> {error, Type}
  end;
validate([{Type, _}|_]) -> {error, Type}.

min_gap({Y, M, D, HH, MM, SS}) ->
  lists:max([gap(Y, 364*24*60*60),
             gap(M, 28*24*60*60),
             gap(D, 24*60*60),
             gap(HH, 60*60),
             gap(MM, 60),
             gap(SS, 1)]).

gap(X, V) when is_atom(X) ->
  case binary:split(bucs:to_binary(X), <<"/">>) of
    [<<"*">>, D] -> bucs:to_integer(D) * V;
    _ -> 1
  end;
gap(_, _) -> 1.

find_next({day, monday, _}) -> [monday];
find_next({day, tuesday, _}) -> [tuesday];
find_next({day, wednesday, _}) -> [wednesday];
find_next({day, thursday, _}) -> [thursday];
find_next({day, friday, _}) -> [friday];
find_next({day, saturday, _}) -> [saturday];
find_next({day, sunday, _}) -> [sunday];
find_next({T, X, Y}) when is_atom(X) ->
  N = case binary:split(bucs:to_binary(X), <<"/">>) of
        [<<"*">>] -> 1;
        [<<"*">>, D] -> bucs:to_integer(D)
      end,
  lists:usort(first_type(T, N) ++ next_type(T, N, [Y + (I * N) || I <- lists:seq(0, 2)]));
find_next({_, LX, _}) when is_list(LX) ->
  LX;
find_next({_, X, _}) when is_integer(X)->
  [X].

next_type(year, 1, L) -> L;
next_type(month, 1, L) -> [fmod(X , 12) || X <- L];
next_type(day, 1, L) -> [fmod(X , 31) || X <- L];
next_type(hour, 1, L) -> [X rem 24 || X <- L];
next_type(minute, 1, L) -> [X rem 60 || X <- L];
next_type(year, D, L) -> [X + D || X <- L];
next_type(month, D, L) -> [fmod(X + D, 12) || X <- L];
next_type(day, D, L) -> [fmod(X + D, 31) || X <- L];
next_type(hour, D, L) -> [(X + D) rem 24 || X <- L];
next_type(minute, D, L) -> [(X + D) rem 60 || X <- L];
next_type(second, D, L) -> [(X + D) rem 60 || X <- L].

first_type(year, _) -> [];
first_type(month, _) -> [];
first_type(day, _) -> [];
first_type(_, 1) -> [0];
first_type(_, N) -> [N].

fmod(X, N) when X < N -> X;
fmod(X, N) -> (X rem N) + 1.

real_dates(Dates) ->
  real_dates(Dates, []).

real_dates([], Result) -> lists:reverse(Result);
real_dates([{_, _, Day} = Date |Rest], Result) when is_integer(Day) ->
  real_dates(Rest, [Date|Result]);
real_dates([{Year, Month, Day}|Rest], Result) when is_atom(Day) ->
  real_dates(Rest, real_dates(Year, Month, Day) ++ Result).

real_dates(Year, Month, Day) ->
  FirstDayOfWeekForMonth = calendar:day_of_the_week(Year, Month, 1),
  WantedDay = day(Day),
  FirstWantedDay = if
                     FirstDayOfWeekForMonth =< WantedDay ->
                       WantedDay - FirstDayOfWeekForMonth + 1;
                     true ->
                       7 - FirstDayOfWeekForMonth + 1 + WantedDay
                   end,
  FirstDate = {Year, Month, FirstWantedDay},
  get_all_dates([FirstDate]).

day(tuesday) -> 2;
day(wednesday) -> 3;
day(thursday) -> 4;
day(friday) -> 5;
day(saturday) -> 6;
day(sunday) -> 7;
day(_) -> 1.

get_all_dates([{Year, Month, Day}|_] = Result) ->
  case calendar:valid_date(Year, Month, Day + 7) of
    true ->
      get_all_dates([{Year, Month, Day + 7}|Result]);
    false ->
      lists:reverse(Result)
  end.

get_date(MinGap, {Date, _} = DateTime, From, Result) ->
  DateTimeToSecond = calendar:datetime_to_gregorian_seconds(DateTime),
  FromToSecond = calendar:datetime_to_gregorian_seconds(From),
  case calendar:valid_date(Date) of
    false -> Result;
    true ->
      case Result of
        undefined ->
          if
            DateTimeToSecond >= (FromToSecond + MinGap) ->
              DateTime;
            true ->
              Result
          end;
        _ ->
          ResultToSecond = calendar:datetime_to_gregorian_seconds(Result),
          if
            (ResultToSecond > DateTimeToSecond)
            and (DateTimeToSecond >= (FromToSecond + MinGap))
            ->
              DateTime;
            true ->
              Result
          end
      end
  end.

