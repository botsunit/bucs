-module(bucos).

-export([run/1, run/2]).

% @equiv run(Cmd, 5000)
-spec run(string() | binary()) -> term().
run(Cmd) ->
  run(Cmd, 5000).

% @doc
% Execute the given shell command
% @end
-spec run(string() | binary(), integer()) -> {ok, term()} | {error, term()}.
run(Cmd, Timeout) ->
  Port = erlang:open_port({spawn, bucs:to_string(Cmd)},[exit_status]),
  loop(Port,[], Timeout).

loop(Port, Data, Timeout) ->
  receive
    {Port, {data, NewData}} -> loop(Port, Data++NewData, Timeout);
    {Port, {exit_status, 0}} -> {ok, Data};
    {Port, {exit_status, S}} -> {error, S}
  after
    Timeout ->
      {error, timeout}
  end.

