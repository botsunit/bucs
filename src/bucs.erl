-module(bucs).

-include_lib("eunit/include/eunit.hrl").
-include("../include/bucs.hrl").

-export([
         type/1,
         is_type/2,
         to_atom/1,
         to_list/1,
         to_string/1,
         to_binary/1,
         to_integer/1,
         to_float/1,
         to_float/2,
         to_term/1,
         to/2,
         as/2,
         module_exist/1,
         module_exists/1,
         function_exist/3,
         function_exists/3,
         apply/4,
         apply/3,
         apply/2,
         is_string/1,
         is_kw_list/1,
         compare_as_list/2,
         compare_as_string/2,
         compare_as_atom/2,
         compare_as_integer/2,
         compare_as_binary/2,
         pipecall/1,
         match/2,
         call/3,
         blank/1,
         present/1,
         default_to/2,
         eval/1,
         eval/2
        ]).

to(atom, Data) -> to_atom(Data);
to(list, Data) -> to_list(Data);
to(string, Data) -> to_string(Data);
to(binary, Data) -> to_binary(Data);
to(integer, Data) -> to_integer(Data);
to(float, Data) -> to_float(Data);
to(term, Data) -> to_term(Data);
to(_, Data) -> Data.

as(Type, Data) -> to(type(Type), Data).

type(Data) when is_atom(Data) -> atom;
type(Data) when is_list(Data) ->
  case is_string(Data) of
    true -> string;
    false -> list
  end;
type(Data) when is_binary(Data) -> binary;
type(Data) when is_float(Data) -> float;
type(Data) when is_integer(Data) -> integer;
type(Data) when is_pid(Data) -> pid;
type(Data) when is_reference(Data) -> reference;
type(Data) when is_port(Data) -> port;
type(Data) when is_tuple(Data) -> tuple;
type(Data) when is_map(Data) -> map;
type(Data) when is_function(Data) -> function;
type(Data) when is_boolean(Data) -> boolean;
type(_) -> undefined.

is_type(Data, Type) -> type(Data) == Type.

% @doc
% Convert the given term to atom
%
% Example:
% <pre>
% atom = bucs:to_atom(atom).
% atom = bucs:to_atom(&lt;&lt;"atom"&gt;&gt;).
% atom = bucs:to_atom("atom").
% </pre>
% @end
to_atom(X) when is_atom(X) ->
  X;
to_atom(X) when is_binary(X); is_bitstring(X) ->
  binary_to_atom(X, utf8);
to_atom(X) when is_list(X) ->
  list_to_atom(X);
to_atom(X) ->
  to_atom(to_list(X)).

% @doc
% Convert the given term to list
%
% Example:
% <pre>
% "list" = bucs:to_list(list).
% "list" = bucs:to_list("list").
% "list" = bucs:to_list(&lt;&lt;"list"&gt;&gt;).
% "123" = bucs:to_list(123).
% "1.20000000000000000000e+01" = bucs:to_list(12.0).
% "true" = bucs:to_list(true).
% "false" = bucs:to_list(false).
% </pre>
% @end
to_list(true) ->
  "true";
to_list(false) ->
  "false";
to_list(V) when is_atom(V) ->
  atom_to_list(V);
to_list(V) when is_list(V) ->
  V;
to_list(V) when is_integer(V) ->
  integer_to_list(V);
to_list(V) when is_float(V) ->
  float_to_list(V);
to_list(V) when is_binary(V); is_bitstring(V) ->
  binary_to_list(V);
to_list(V) when is_tuple(V) ->
  [element(I, V) || I <- lists:seq(1, tuple_size(V))];
to_list(V) when is_pid(V) ->
  pid_to_list(V);
to_list(V) when is_reference(V) ->
  erlang:ref_to_list(V).

% @doc
% Convert the given term to string
% @end
to_string(V) when is_binary(V);
                  is_bitstring(V);
                  is_atom(V);
                  is_pid(V);
                  is_reference(V)->
  lists:flatten(to_list(V));
to_string(V) ->
  case is_string(V) of
    true -> V;
    false ->
      lists:flatten(io_lib:format("~p", [V]))
  end.

% @doc
% Convert the given term to binary
%
% Example:
% <pre>
% &lt;&lt;"list"&gt;&gt; = bucs:to_binary(list).
% &lt;&lt;"list"&gt;&gt; = bucs:to_binary("list").
% &lt;&lt;"list"&gt;&gt; = bucs:to_binary(&lt;&lt;"list"&gt;&gt;).
% &lt;&lt;"123"&gt;&gt; = bucs:to_binary(123).
% &lt;&lt;"1.20000000000000000000e+01"&gt;&gt; = bucs:to_binary(12.0).
% &lt;&lt;"true"&gt;&gt; = bucs:to_binary(true).
% &lt;&lt;"false"&gt;&gt; = bucs:to_binary(false).
% </pre>
% @end
to_binary(V) when is_binary(V); is_bitstring(V) ->
  V;
to_binary(V) when is_float(V) ->
  iolist_to_binary(to_string(V));
to_binary(V) ->
  iolist_to_binary(to_list(V)).

% @doc
% Convert the given term to integer
%
% Example
%<pre>
% 123 = bucs:to_integer(123).
% 123 = bucs:to_integer("123").
% 123 = bucs:to_integer(&lt;&lt;"123"&gt;&gt;).
% 123 = bucs:to_integer('123').
% 123 = bucs:to_integer(123.456).
% </pre>
% @end
to_integer(I) when is_integer(I) ->
  I;
to_integer(I) when is_list(I) ->
  try
    list_to_integer(I)
  catch
    _:_ -> to_integer(to_float(I))
  end;
to_integer(I) when is_binary(I); is_bitstring(I) ->
  try
    binary_to_integer(I)
  catch
    _:_ -> to_integer(to_float(I))
  end;
to_integer(I) when is_atom(I) ->
  to_integer(atom_to_list(I));
to_integer(I) when is_float(I) ->
  to_integer(float_to_list(I, [{decimals, 0}])).

% @doc
% Convert the given term to float
%
% Example
%<pre>
% 123.45 = bucs:to_float(123.45).
% 123.45 = bucs:to_float("123.45").
% 123.45 = bucs:to_float(&lt;&lt;"123.45"&gt;&gt;).
% 123.45 = bucs:to_float('123.45').
% 123.0 = bucs:to_float(123).
% </pre>
% @end
to_float(Value) when is_integer(Value) ->
  float(Value);
to_float(Value) when is_float(Value) ->
  Value;
to_float(Value) when is_list(Value) ->
  case string:to_float(Value) of
    {error, no_float} -> float(list_to_integer(Value));
    {F, _} -> F
  end;
to_float(Value) when is_binary(Value) ->
  to_float(binary_to_list(Value));
to_float(Value) when is_atom(Value) ->
  to_float(atom_to_list(Value)).

% @doc
% Convert the given term to float, with the given precision
%
% Example
%<pre>
% 123.457 = bucs:to_float(123.45678i, 3).
% 123.457 = bucs:to_float("123.45678", 3).
% 123.457 = bucs:to_float(&lt;&lt;"123.45678"&gt;&gt;, 3).
% 123.457 = bucs:to_float('123.45678', 3).
% 123.0 = bucs:to_float(123, 3).
% </pre>
% @end
to_float(Value, Precision) ->
  to_float(float_to_list(to_float(Value), [{decimals, Precision}])).

% @doc
% Convert the given value to term
%
% Example
% <pre>
%
% </pre>
% @end
to_term(Value) ->
  try
    Value0 = to_string(Value),
    Value1 = case lists:reverse(Value0) of
               [$.|_] -> Value0;
               _ -> Value0 ++ "."
             end,
    {ok, Tokens, _} = erl_scan:string(Value1),
    case erl_parse:parse_term(Tokens) of
      {ok, Term} -> {ok, Term};
      {error, {_, _, Reason}} -> {error, Reason}
    end
  catch
    _:Error ->
      {error, Error}
  end.

% @deprecated
module_exist(Module) ->
  module_exists(Module).

% @doc
% Check if the given module exist
% @end
module_exists(Module) ->
  case is_atom(Module) of
    true ->
      try Module:module_info() of
        _InfoList ->
          true
      catch
        _:_ ->
          false
      end;
    false ->
      false
  end.

% @deprecated
function_exist(Module, Function, Arity) ->
  function_exists(Module, Function, Arity).

% @doc
% Check if the given function exist
% @end
function_exists(Module, Function, Arity) ->
  case code:ensure_loaded(Module) of
    {module, Module} ->
      erlang:function_exported(Module, Function, Arity);
    _ ->
      false
  end.

% @doc
% @end
apply(Module, Function, Args, Default) ->
  case function_exists(Module, Function, length(Args)) of
    true ->
      erlang:apply(Module, Function, Args);
    false ->
      Default
  end.

% @doc
% @end
apply(Fun, Args, Default) when is_function(Fun),
                               is_list(Args) ->
  case bucs:apply(Fun, Args) of
    {ok, Result} -> Result;
    error -> Default
  end;
apply(Module, Function, Args) when is_atom(Module),
                                   is_atom(Function),
                                   is_list(Args) ->
  try
    {ok, erlang:apply(Module, Function, Args)}
  catch
    _:_ ->
      error
  end.

apply(Fun, Args) ->
  try
    {ok, erlang:apply(Fun, Args)}
  catch
    _:_ ->
      error
  end.

% @doc
% Check if the given value is a string
% @end
is_string(V) when is_list(V) ->
  io_lib:printable_list(V) orelse io_lib:printable_latin1_list(V) orelse io_lib:printable_unicode_list(V);
is_string(_) -> false.

% @doc
% Wheck is the given value is a keyword list
% @end
is_kw_list(V) when is_list(V) ->
  lists:all(fun
              ({_, _}) -> true;
              (_) -> false
            end, V);
is_kw_list(_) -> false.

% @doc
% Return true if <tt>A</tt> match <tt>B</tt>. false otherwise.
% @end
match(A, B) ->
  case A of
    B -> true;
    _ -> false
  end.

compare_as_list(V1, V2) ->
  compare_as(fun to_list/1, V1, V2).
compare_as_string(V1, V2) ->
  compare_as(fun to_string/1, V1, V2).
compare_as_atom(V1, V2) ->
  compare_as(fun to_atom/1, V1, V2).
compare_as_integer(V1, V2) ->
  compare_as(fun to_integer/1, V1, V2).
compare_as_binary(V1, V2) ->
  compare_as(fun to_binary/1, V1, V2).

compare_as(Fun, V1, V2) ->
  V11 = Fun(V1),
  V21 = Fun(V2),
  if
    V11 < V21 -> -1;
    V11 =:= V21 -> 0;
    true -> 1
  end.

% @doc
% Pipe fun call
%
% Example:
% <pre>
% Add = math:pow(7, 3),
% Log = math:log(Add),
% Mul = multiplication(Log, 7),
% Res = addition(Mul, 7).
%
% % With bucs:pipecall/1 :
% Res = bucs:pipecall([
%                      {fun math:pow/2, [7, 3]},
%                      fun math:log/1,
%                      {fun multiplication/2, [7]},
%                      {fun addition/2, [7]}
%                     ]).
% </pre>
% @end
pipecall([{Call, Args}|Rest]) ->
  pipecall(Rest, erlang:apply(Call, Args));
pipecall([Call|Rest]) ->
  pipecall(Rest, erlang:apply(Call, [])).

pipecall([], Out) ->
  Out;
pipecall([{Call, Args}|Rest], Out) ->
  pipecall(Rest, erlang:apply(Call, [Out|Args]));
pipecall([Call|Rest], Out) ->
  pipecall([{Call, []}|Rest], Out).

% @doc
% Returns the result of applying Function in Module to Args. The applied
% function must be exported from Module. The arity of the function is the length of Args.
%
% Return <tt>{error, undefined_function}</tt> if the applied function is not exported.
% @end
-spec call(Module :: module(), Function :: atom(), Args :: [term()]) -> term() | {error, undefined_function}.
call(Module, Function, Args) ->
  case erlang:function_exposted(Module, Function, length(Args)) of
    true ->
      erlang:apply(Module, Function, Args);
    false ->
      {error, undefined_function}
  end.

% @doc
% @end
blank([]) -> true;
blank({}) -> true;
blank(<<>>) -> true;
blank(M) when is_map(M) -> maps:size(M) == 0;
blank(nil) -> true;
blank(undefined) -> true;
blank(false) -> true;
blank(X) ->
  try
    binary:replace(bucs:to_binary(X),
                   [<<" ">>, <<"\r">>, <<"\n">>, <<"\t">>],
                   <<>>,
                   [global]) == <<>>
  catch
    _:_ -> false
  end.

% @doc
% @end
present(X) ->
  not blank(X).

% @doc
% @end
default_to(X, Default) ->
  case blank(X) of
    true ->
      Default;
    _ ->
      X
  end.

eval(Value) ->
  eval(Value, []).

eval(Value, Environ) ->
  Value0 = to_string(Value),
  Value1 = case lists:reverse(Value0) of
             [$.|_] -> Value0;
             _ -> Value0 ++ "."
           end,
  case erl_scan:string(Value1) of
    {ok, Scanned, _} ->
      case erl_parse:parse_exprs(Scanned) of
        {ok, Parsed} ->
          erl_eval:exprs(Parsed, Environ);
        Error ->
          Error
      end;
    Error ->
      Error
  end.

