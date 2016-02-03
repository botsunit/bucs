-module(bucinet).

-export([
         to_ip/1,
         ip_to_string/1,
         ip_to_binary/1,
         active_ip/0,
         active_ips/0,
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
% Return the first active IP (or loopback if none).
% @end
-spec active_ip() -> inet:ip4_address().
active_ip() ->
  case get_active_ips(get_iflist()) of
    [] -> loopback();
    [Ip|_] -> Ip
  end.

% @doc
% Return all actives IPs.
% @end
-spec active_ips() -> [inet:ip4_address()].
active_ips() ->
  get_active_ips(get_iflist()).

% @doc
% Return the loopback IP.
% @end
-spec loopback() -> inet:ip4_address().
loopback() ->
  case get_loopback(get_iflist()) of
    [] -> error;
    [L|_] -> L
  end.

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

get_active_ips(If_list) ->
  get_ips([A || A <- If_list, inet:ifget(A,[addr]) /= {ok,[{addr,{127,0,0,1}}]}, filter_networkcard(list_to_binary(A))]).

get_iflist() ->
  {ok, IfList} = inet:getiflist(),
  IfList.

filter_networkcard(<<"vboxnet", _R/binary>>) ->
 false;
filter_networkcard(<<"vnic", _R/binary>>) ->
  false;
filter_networkcard(<<"vmnet", _R/binary>>) ->
  false;
filter_networkcard(<<"docker", _R/binary>>) ->
  false;
filter_networkcard(_) ->
  true.

get_ips(A) ->
  get_ips(A, []).

get_ips([], R) ->
  R;
get_ips([If|Rest], R) ->
  case inet:ifget(If, [addr]) of
    {ok, []} -> get_ips(Rest, R);
    {_, [{_, Ip}]} -> get_ips(Rest, [Ip|R])
  end.

get_loopback(If_list) ->
  get_ips([A || A <- If_list, inet:ifget(A,[addr]) == {ok,[{addr,{127,0,0,1}}]}]).

