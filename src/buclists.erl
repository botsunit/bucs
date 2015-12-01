-module(buclists).

-export([
         pipemap/2
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
