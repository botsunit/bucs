-module(bucinet).

-export([
         to_ip/1,
         ip_to_string/1,
         ip_to_binary/1,
         active_ip/0,
         loopback/0,
         is_ip/1
        ]).

% @doc
% Return a <tt>inet:ip4_address()</tt> from a string or a binary.
% @end
-spec to_ip(binary() | string()) -> inet:ip4_address() | error.
to_ip(IP) when is_binary(IP) ->
  to_ip(bucs:to_list(IP));
to_ip(IP) when is_list(IP) ->
  try
    case [bucs:to_integer(X) || X <- string:tokens(IP, ".")] of
      [A, B, C, D] when A >= 0, A =< 255,
                        B >= 0, B =< 255,
                        C >= 0, C =< 255,
                        D >= 0, D =< 255 -> 
        {A, B, C, D};
      _ -> error
    end
  catch
    _:_ ->
      error
  end;
to_ip(_) ->
  error.

% @doc
% Return a string for the given <tt>inet:ip4_address()</tt>
% @end
-spec ip_to_string(inet:ip4_address()) -> string() | error.
ip_to_string({A, B, C, D}) when A >= 0, A =< 255,
                                B >= 0, B =< 255,
                                C >= 0, C =< 255,
                                D >= 0, D =< 255 -> 
  lists:flatten(io_lib:format("~B.~B.~B.~B", [A, B, C, D]));
ip_to_string(_) ->
  error.

% @doc
% Return a binary for a given <tt>inet:ip4_address()</tt>
% @end
-spec ip_to_binary(inet:ip4_address()) -> binary() | error.
ip_to_binary(IP) ->
  case ip_to_string(IP) of
    error -> error;
    Str -> bucs:to_binary(Str)
  end.

% @doc
% Return the first active IP.
% @end
-spec active_ip() -> inet:ip4_address().
active_ip() ->
  get_active_ip(get_iflist()).

% @doc
% Return the loopback IP.
% @end
-spec loopback() -> inet:ip4_address().
loopback() ->
  get_loopback(get_iflist()).

% @doc
% Return true if the given parameter is an IP.
% @end
-spec is_ip(term()) -> true | false.
is_ip({A, B, C, D}) when A >= 0, A =< 255,
                         B >= 0, B =< 255,
                         C >= 0, C =< 255,
                         D >= 0, D =< 255 ->
  true;
is_ip(IP) ->
  error =/= to_ip(IP).

% privates

get_active_ip(If_list) ->
  get_ip([A || A <- If_list, inet:ifget(A,[addr]) /= {ok,[{addr,{127,0,0,1}}]}, filter_networkcard(list_to_binary(A))]).

get_iflist() ->
  {ok, IfList} = inet:getiflist(),
  IfList.

filter_networkcard(<<"vnic", _R/binary>>) ->
  false;
filter_networkcard(<<"vmnet", _R/binary>>) ->
  false;
filter_networkcard(_) ->
  true.

get_ip([]) ->
  loopback();
get_ip([If]) ->
  case inet:ifget(If, [addr]) of
    {ok, []} -> loopback();
    {_, [{_, Ip}]} -> Ip
  end.

get_loopback(If_list) ->
  get_ip([A || A <- If_list, inet:ifget(A,[addr]) == {ok,[{addr,{127,0,0,1}}]}]).

