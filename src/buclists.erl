-module(buclists).

-export([
         pipemap/2,
         keyfind/3,
         keyfind/4,
         keyfind/5,
         keyufind/3,
         keyufind/4,
         delete_if/2,
         merge_keylists/3
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

% @equiv keyufind(Key, N, TupleList, false)
-spec keyufind(term(), integer(), [tuple()]) -> term().
keyufind(Key, N, TupleList) ->
  keyufind(Key, N, TupleList, false).

% @doc
% @end
-spec keyfind(term(), integer(), [tuple()], term()) -> term().
keyfind(Key, N, TupleList, Default) ->
  case lists:keyfind(Key, N, TupleList) of
    {Key, Value} -> Value;
    false -> Default;
    Tuple -> Tuple
  end.

keyfind(Key, N, TupleList, M, Default) ->
  case lists:keyfind(Key, N, TupleList) of
    false -> Default;
    Tuple -> if
               tuple_size(Tuple) >= M -> element(M, Tuple);
               true -> Default
             end
  end.

% @doc
% @end
-spec keyufind(term(), integer(), [tuple()], term()) -> term().
keyufind(Key, N, TupleList, Default) ->
  keyfind(bucs:to_atom(Key), N, lists:keymap(fun bucs:to_atom/1, N, TupleList), Default).


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

%% @doc
%% Merge the two keylists.
%%
%% Example:
%% <pre>
%% Args = [{a, 1}, {b, 2}],
%% Default = [{b, 3}, {c, 4}],
%% elists:merge_keylists(1, Args, Default),
%%   #=> [{c, 4}, {a, 1}, {b, 2}]
%% </pre>
%% @end
merge_keylists(_, [], TupleList2) ->
  TupleList2;
merge_keylists(N, [Tuple|Rest], TupleList2) when
    is_integer(N), is_list(TupleList2), is_tuple(Tuple), is_list(Rest) ->
  Key = element(N, Tuple),
  TupleList3 = case lists:keysearch(Key, N, TupleList2) of
    {value, _} -> lists:keydelete(Key, N, TupleList2);
    false -> TupleList2
  end,
  merge_keylists(N, Rest, TupleList3 ++ [Tuple]);
merge_keylists(N, [Tuple|Rest], TupleList2) when
    is_integer(N), is_list(TupleList2), is_list(Rest)->
  merge_keylists(N, Rest, TupleList2 ++ [Tuple]).

