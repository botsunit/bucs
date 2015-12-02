-module(bucbinary).

-export([
         join/2
        ]).

%% @doc
%% join a list of binaries with the given separator
%%
%% Example:
%%
%% <pre lang="erlang">
%% &lt;&lt;"toto-tata-titi"&gt;&gt; = ebinary:join([&lt;&lt;"toto"&gt;&gt;, &lt;&lt;"tata"&gt;&gt;, &lt;&lt;"titi"&gt;&gt;], &lt;&lt;"-"&gt;&gt;).
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

