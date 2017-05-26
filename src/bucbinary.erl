-module(bucbinary).

-export([
         join/2
         , trim/2
         , is_integer/1
         , is_float/1
         , are_integers/1
         , are_floats/1
        ]).

%% @doc
%% join a list of binaries with the given separator
%%
%% Example:
%%
%% <pre lang="erlang">
%% &lt;&lt;"toto-tata-titi"&gt;&gt; = bucbinary:join([&lt;&lt;"toto"&gt;&gt;, &lt;&lt;"tata"&gt;&gt;, &lt;&lt;"titi"&gt;&gt;], &lt;&lt;"-"&gt;&gt;).
%% </pre>
%% @end
-spec join([binary()], binary()) -> binary().
join([], _) ->
  <<>>;
join(L, S) when is_list(L), is_binary(S) ->
  join(L, S, <<>>);
join(B, _) when is_binary(B) ->
  B.
join([], _, Acc) ->
  Acc;
join([E|R], S, <<>>) ->
  join(R, S, bucs:to_binary(E));
join([E|R], S, Acc) ->
  join(R, S, <<Acc/binary, S/binary, (bucs:to_binary(E))/binary>>).

- spec trim(binary(), left | right | both) -> binary().
trim(Binary, left) ->
  trim_left(Binary);
trim(Binary, right) ->
  trim_right(Binary);
trim(Binary, both) ->
  trim_left(trim_right(Binary)).

trim_left(<<C, Rest/binary>>) when C =:= $\s orelse
                                   C =:= $\n orelse
                                   C =:= $\r orelse
                                   C =:= $\t ->
  trim_left(Rest);
trim_left(Binary) -> Binary.

trim_right(Binary) ->
  trim_right(Binary, size(Binary)-1).

trim_right(Binary, Size) ->
  case Binary of
    <<Rest:Size/binary, C>> when C =:= $\s
                                 orelse C =:= $\t
                                 orelse C =:= $\n
                                 orelse C =:= $\r ->
      trim_right(Rest, Size - 1);
    Other ->
      Other
  end.

-spec are_floats([binary()]) -> true | false.
are_floats([]) ->
  false;
are_floats(List) ->
  are_floats(List, true).
are_floats([], Acc) ->
  Acc;
are_floats([E|Rest], Acc) ->
  are_floats(Rest, bucbinary:is_float(E) andalso Acc).

-spec is_float(binary()) -> true | false.
is_float(<<>>) ->
  false;
is_float(Data) ->
  case binary:split(Data, [<<".">>]) of
    [_, _] = V ->
      are_integers(V);
    _ ->
      false
  end.

-spec are_integers([binary()]) -> true | false.
are_integers([]) ->
  false;
are_integers(List) ->
  are_integers(List, true).
are_integers([], Acc) ->
  Acc;
are_integers([E|Rest], Acc) ->
  are_integers(Rest, bucbinary:is_integer(E) andalso Acc).

-spec is_integer(binary()) -> true | false.
is_integer(<<>>) ->
  false;
is_integer(Data) ->
  do_is_integer(Data).


do_is_integer(<<>>) ->
  true;
do_is_integer(<<"0", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"1", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"2", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"3", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"4", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"5", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"6", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"7", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"8", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(<<"9", Rest/binary>>) ->
  do_is_integer(Rest);
do_is_integer(_) ->
  false.
