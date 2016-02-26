-module(bucos).

-export([
         run/1,
         run/2,
         in/2,
         in/3
        ]).
-define(TIMEOUT, 5000).
-define(DEFAULT_RETURN_TYPE, combined).

-type options() :: {timeout,integer()} | stdout_on_error | {return, combined|list, all|last|integer()|[integer()]}.

% @equiv run(Cmd, 5000)
-spec run(string() | binary()) -> term().
run(Cmd) ->
  run(Cmd, ?TIMEOUT).

% @doc
% Execute the given shell command, waiting at most for a given timeout before returning
% <tt>Options</tt> may contain:
% <ul>
% <li><tt>stdout_on_error</tt> : To get standard output in the result, in case of error.</li>
% <li><tt>display_stdout</tt> : Display stdout.</li>
% <li><tt>{timeout, integer()}</tt> : To set a maximum time to wait for, before returning with a <tt>{error,timeout}</tt> result.</li>
% <li><tt>{return, list|combined, all|last|integer()|[integer()]}</tt> : To specify output collection.</li>
% <li><tt>{cd, string() | binary()}</tt> : Change directory before run command.</li>
% <li><tt>{env, [{string(), string() | false}]}</tt> :  The environment of the started process is extended using the environment specifications.</li>
% </ul>
% Note: If more than one shell commands are "chained" in the given string, only the first one is executed.
% @end
-spec run([string() | binary()], integer() | [options()]) -> {ok, string()|[string()]} | {error, integer()} | {error, integer(), string()}.
run(Cmd, Timeout) when is_integer(Timeout) ->
  run(Cmd, [{timeout, Timeout}]);
run(Cmd, Options) when is_list(Options) ->
  case is_binary(Cmd) orelse bucs:is_string(Cmd) of
    true ->
      Timeout = buclists:keyfind(timeout, 1, Options, ?TIMEOUT),
      StdoutOnError = lists:member(stdout_on_error, Options),
      DisplayStdout = lists:member(display_stdout, Options),
      Port = erlang:open_port({spawn, bucs:to_string(Cmd)}, run_options(Options, [exit_status, stderr_to_stdout])),
      loop(Port, [], Timeout, StdoutOnError, DisplayStdout);
    _ ->
      run_all(Cmd, Options, [])
  end;
run(_,_) ->
  error(badarg).

run_options([], Acc) ->
  Acc;
run_options([CD = {cd, _}|Options], Acc) ->
  run_options(Options, [CD|Acc]);
run_options([ENV = {env, _}|Options], Acc) ->
  run_options(Options, [ENV|Acc]);
run_options([_|Options], Acc) ->
  run_options(Options, Acc).

loop(Port, Data, Timeout, StdoutOnError, DisplayStdout) ->
  receive
    {Port, {data, NewData}} ->
      if
        DisplayStdout -> io:format("~s", [NewData]);
        true -> ok
      end,
      loop(Port, Data++NewData, Timeout, StdoutOnError, DisplayStdout);
    {Port, {exit_status, 0}} -> {ok, Data};
    {Port, {exit_status, S}} ->
      if
        StdoutOnError -> {error, S, Data};
        true -> {error, S}
      end
  after
    Timeout ->
      {error, timeout}
  end.

run_all([], Options, Acc) ->
  Results = lists:reverse(Acc),
  case lists:keyfind(return, 1, Options) of
    {return, Type, all} ->
      {ok, results(Type, Results)};
    {return, Type, last} ->
      {ok, results(Type, [lists:nth(length(Results), Results)])};
    {return, Type, N} when is_integer(N) ->
      {ok, results(Type, nths([N], Results))};
    {return, Type, L} when is_list(L) ->
      {ok, results(Type, nths(L, Results))};
    _ ->
      {ok, results(?DEFAULT_RETURN_TYPE, Results)}
  end;
run_all([Cmd|Cmds], Options, Acc) ->
  case run(Cmd, Options) of
    {ok, Data} -> run_all(Cmds, Options, [Data|Acc]);
    E -> E
  end.

nths(Elements, List) ->
  lists:reverse(
    lists:foldl(fun
                  (N, Acc) when N =< length(List), N >= 0 ->
                  [lists:nth(N, List)|Acc];
                (_, Acc) ->
                  Acc
                end, [], Elements)).

results(combined, List) ->
  lists:flatten(List);
results(_, List) -> List.

%% @doc
%% Execute the given function function in the given path.
%%
%% Example :
%%
%% <pre lang="erlang">
%% eos:in("/tmp", fun() ->
%%   ?assertMatch({ok, "/tmp"}, file:get_cwd())
%%   end).
%% </pre>
%% @end
in(Path, Fun, Args) when is_function(Fun) ->
  case file:get_cwd() of
    {ok, Dir} ->
      case file:set_cwd(Path) of
        ok ->
          Result = apply(Fun, Args),
          case file:set_cwd(Dir) of
            ok -> Result;
            E -> E
          end;
        E -> E
      end;
    E -> E
  end.

%% @equiv in(Path, Fun, [])
in(Path, Fun) when is_function(Fun) ->
  in(Path, Fun, []).
