-module(bucmaps).

-export([from_list/1, from_list/2, to_list/1, to_list/2]).

-spec from_list(List :: list()) -> map().
from_list(List) when is_list(List) ->
  from_list(List, all).

-spec from_list(List :: list(), Deep :: integer() | all) -> map().
from_list(List, Deep) when is_list(List),
                           (is_integer(Deep) orelse Deep =:= all) ->
  from_list(List, Deep, 1).

-spec to_list(Map :: map()) -> list().
to_list(Map) when is_map(Map) ->
  to_list(Map, all).

-spec to_list(Map :: map(), Deep :: integer() | all) -> list().
to_list(Map, Deep) when is_map(Map),
                        (is_integer(Deep) orelse Deep =:= all) ->
  to_list(Map, Deep, 1).

from_list(List, Deep, Level) ->
  Map = maps:from_list(List),
  if
    Level < Deep orelse Deep =:= all ->
      maps:map(fun(_, V) ->
                   case bucs:is_kw_list(V) of
                     true ->
                       from_list(V, Deep, Level + 1);
                     _ ->
                       V
                   end
               end, Map);
    true ->
      Map
  end.

to_list(Map, Deep, Level) ->
  List = maps:to_list(Map),
  if
    Level < Deep orelse Deep =:= all ->
      lists:map(fun
                  ({K, V}) when is_map(V) ->
                    {K, to_list(V, Deep, Level + 1)};
                  (E) ->
                    E
                end, List);
    true ->
      List
  end.

