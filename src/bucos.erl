-module(bucos).

-export([run/1, run/2]).
-define(TIMEOUT, 5000).

-type options() :: {timeout,integer()} | stdout_on_error. %TODO: | {return, combined|last|integer()|[integer()]}.

% @equiv run(Cmd, 5000)
-spec run(string() | binary()) -> term().
run(Cmd) ->
  run(Cmd, ?TIMEOUT).

% @doc
% Execute the given shell command, waiting at most for a given timeout before returning
% "Options" may contain:
% - stdout_on_error : To get standard output in the result, in case of error.
% - {timeout, integer()} : To set a maximum time to wait for, before returning with a {error,timeout} result
% Note: If more than one shell commands are "chained" in the given string, only the first one is executed.
% @end
-spec run(string() | binary(), integer() | [options()]) -> {ok, term()} | {error, term()} | {error, term(), string()}.
run(Cmd, Timeout) when is_integer(Timeout) ->
  Port = erlang:open_port({spawn, bucs:to_string(Cmd)},[exit_status]),
  loop(Port,[], Timeout);
run(Cmd, Options) when is_list(Options) ->
  Timeout = get_timeout(Options),
  Port = erlang:open_port({spawn, bucs:to_string(Cmd)},[exit_status]),
  case get_stdout_on_error(Options) of
    true -> verbose_loop(Port, [], Timeout);
    false -> loop(Port, [], Timeout)
  end;
run(_,_) ->
  error(badarg).

get_timeout([]) -> ?TIMEOUT;
get_timeout([{timeout,TimeValueMs}|_T]) -> TimeValueMs;
get_timeout([_H|T]) -> get_timeout(T).

get_stdout_on_error([]) -> false;
get_stdout_on_error([stdout_on_error|_T]) -> true;
get_stdout_on_error([_H|T]) -> get_stdout_on_error(T).

verbose_loop(Port, Data, Timeout) ->
  receive
    {Port, {data, NewData}} -> verbose_loop(Port, Data++NewData, Timeout);
    {Port, {exit_status, 0}} -> {ok, Data};
    {Port, {exit_status, S}} -> {error, S, Data}
  after
    Timeout ->
      {error, timeout}
  end.

loop(Port, Data, Timeout) ->
  case verbose_loop(Port, Data, Timeout) of
    {error,S,_D} -> {error,S};
    OtherResults -> OtherResults
  end.

