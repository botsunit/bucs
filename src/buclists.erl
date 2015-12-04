-module(buclists).

-export([
         pipemap/2,
         keyfind/3,
         keyfind/4,
         delete_if/2
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

% @equiv keyfind(Key, N, TupleList, false)
-spec keyfind(term(), integer(), [tuple()]) -> term().
keyfind(Key, N, TupleList) ->
  keyfind(Key, N, TupleList, false).

% @doc
% @end
-spec keyfind(term(), integer(), [tuple()], term()) -> term().
keyfind(Key, N, TupleList, Default) ->
  case lists:keyfind(Key, N, TupleList) of
    {Key, Value} -> Value;
    false -> Default
  end.

%% @doc
%% @end
delete_if(Fun, List) ->
  lists:reverse(lists:foldl(fun(E, Acc) ->
                                case Fun(E) of
                                  true -> Acc;
                                  false -> [E|Acc]
                                end
                            end,
                            [],
                            List)).

