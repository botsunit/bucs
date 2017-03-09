-module(bucbinary).

-export([
         join/2,
         trim/2
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

