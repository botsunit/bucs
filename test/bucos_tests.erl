-module(bucos_tests).

-include_lib("eunit/include/eunit.hrl").

bucos_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_run_returns())
    , ?_test(t_run_returns_with_full_options())
    , ?_test(t_run_error_with_simple_status())
    , ?_test(t_run_timeout_expires())
    , ?_test(t_run_timeout_dont_expire())
    , ?_test(t_run_timeout_as_option_expires())
    ,? _test(t_run_timeout_with_full_options_expires())
    , ?_test(t_run_error_with_text_output())
    % , ?_test(t_run_error_with_full_options())
    , ?_test(t_run_exception_bad_options())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_run_returns() ->
  ?assertMatch({ok, "Hello\n"}, bucos:run("echo Hello")).

t_run_returns_with_full_options() ->
  ?assertMatch({ok, "Hello\n"}, bucos:run("echo Hello",[{timeout,200},stdout_on_error])).

t_run_error_with_simple_status() ->
  ?assertMatch({error, _ }, bucos:run("invalidCommand 2>&1")).

t_run_timeout_expires() ->
  ?assertMatch({error, timeout}, bucos:run("sleep 1", 500)).

t_run_timeout_dont_expire() ->
  ?assertMatch({ok, _}, bucos:run("sleep 1", 1100)).

t_run_timeout_as_option_expires() ->
  ?assertMatch({error, timeout}, bucos:run("sleep 1", [{timeout,500}])).

t_run_timeout_with_full_options_expires() ->
  ?assertMatch({error, timeout}, bucos:run("sleep 1", [stdout_on_error,{timeout,500}])).

t_run_error_with_text_output() ->
  {error,ErrCode,Data} = bucos:run("invalidCommand 2>&1",[stdout_on_error]),
  ?assert(ErrCode /= 0 andalso string:len(Data)>14). % >14: I assume that the faulty command is somewhere inside the error output

% t_run_error_with_full_options() ->
%   {error,ErrCode,Data} = bucos:run("invalidCommand 2>&1",[stdout_on_error,{timeout,1100}]),
%   ?assert(ErrCode /= 0 andalso string:len(Data)>14). % >14: I assume that the faulty command is somewhere inside the error output

t_run_exception_bad_options() ->
  ?assertError(badarg, bucos:run("echo Hello", {crocodile,piranha} )).
