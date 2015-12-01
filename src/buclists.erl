-module(buclists).

-export([
         pipemap/2,
         keyfind/4
        ]).

% @doc
% @end
-spec pipemap(list(), list()) -> list().
pipemap(Funs, List) ->
  lists:map(fun(E) ->
                bucs:pipecall(
                  case Funs of
                    [{Fun, Args}|Rest] ->
                      [{Fun, [E|Args]}|Rest];
                    [Fun|Rest] ->
                      [{Fun, [E]}|Rest]
                  end)
            end, List).

% @doc
% @end
-spec keyfind(term(), integer(), [tuple()], term()) -> term().
keyfind(Key, N, TupleList, Default) ->
  case lists:keyfind(Key, N, TupleList) of
    {Key, Value} -> Value;
    false -> Default
  end.

