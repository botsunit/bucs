-define(record_to_list(Record, Val),
        begin
            Fields = record_info(fields, Record),
            [_Tag| Values] = tuple_to_list(Val),
            lists:zip(Fields, Values)
        end).

-define(record_to_map(Record, Val),
        maps:from_list(?record_to_list(Record, Val))).

-define(list_to_record(Record, List),
        begin
            Fields = record_info(fields, Record),
            [Tag| Values] = tuple_to_list(#Record{}),
            Defaults = lists:zip(Fields, Values),
            L = lists:map(fun ({K,V}) -> proplists:get_value(K, List, V) end, Defaults),
            list_to_tuple([Tag|L])
        end).

-define(map_to_record(Record, Map),
        ?list_to_record(Record, maps:to_list(Map))).

