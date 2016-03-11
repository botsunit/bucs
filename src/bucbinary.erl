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
join([Bin], _) when is_binary(Bin) ->
  Bin;
join(List, Sep) when is_list(List), is_binary(Sep) ->
  lists:foldr(fun
                (<<>>, <<>>) -> <<>>;
                (A, <<>>) -> A;
                (<<>>, B) -> B;
                (A, B) -> <<A/binary, Sep/binary, B/binary>>
              end, <<>>, List);
join(Bin, _) when is_binary(Bin) ->
  Bin.


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

