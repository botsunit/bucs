-module(bucuri).

-export([join/2, join/1]).

% @doc
% Joins two URI paths with URI separator.
% @end
-spec join(string() | binary(), string() | binary()) -> string().
join(A, B) ->
  join([A, B]).

% @doc
% Joins a list of URI paths with URI separator.
% @end
-spec join(list()) -> string().
join(URIs) when is_list(URIs) ->
  Join = join(URIs, [], true),
  case {is_list(Join), start_with_sep(URIs)} of
    {true, true} ->
      "/" ++ Join;
    {false, true} ->
      <<"/", Join/binary>>;
    _ ->
      Join
  end.

join([], Acc, String) ->
  URI = string:join(Acc, "/"),
  if
    String == true -> URI;
    true -> list_to_binary(URI)
  end;
join([C|Rest], Acc, _) when is_binary(C) ->
  join([bucs:to_list(C)|Rest], Acc, false);
join([C], Acc, String) when is_list(C) ->
  join([], Acc ++ [string:strip(C, left, $/)], String);
join([C|Rest], Acc, String) when is_list(C) ->
  join(Rest, Acc ++ [string:strip(C, both, $/)], String).

start_with_sep([[$/|_]|_]) -> true;
start_with_sep([<<"/", _/binary>>|_]) -> true;
start_with_sep(_) -> false.

