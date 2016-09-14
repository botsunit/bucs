-module(bucinet_tests).

-include_lib("eunit/include/eunit.hrl").

bucinet_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_to_ip())
    , ?_test(t_ip_to_string())
    , ?_test(t_ip_to_binary())
    , ?_test(t_active_ip())
    , ?_test(t_loopback())
    , ?_test(t_active_ips())
    , ?_test(t_country())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_to_ip() ->
  ?assertMatch({192, 168, 10, 1},
               bucinet:to_ip("192.168.10.1")),
  ?assertMatch({192, 168, 10, 1},
               bucinet:to_ip(<<"192.168.10.1">>)),
  ?assertMatch({192, 168, 10, 1},
               bucinet:to_ip(<<"192.168.10.1">>)),
  ?assertMatch(error,
               bucinet:to_ip("223.6723.889.1")),
  ?assertMatch(error,
               bucinet:to_ip("This is not an IP")).

t_ip_to_string() ->
  ?assertMatch("192.168.10.1",
               bucinet:ip_to_string({192, 168, 10, 1})),
  ?assertMatch(error,
               bucinet:ip_to_string({192, 317, 10, 1})),
  ?assertMatch(error,
               bucinet:ip_to_string("This is not an IP")),
  ?assertMatch(error,
               bucinet:ip_to_string({192, 168})).

t_ip_to_binary() ->
  ?assertMatch(<<"192.168.10.1">>,
               bucinet:ip_to_binary({192, 168, 10, 1})),
  ?assertMatch(error,
               bucinet:ip_to_binary({192, 317, 10, 1})),
  ?assertMatch(error,
               bucinet:ip_to_binary("This is not an IP")),
  ?assertMatch(error,
               bucinet:ip_to_binary({192, 168})).

t_active_ip() ->
  ?assert(bucinet:is_ip(bucinet:active_ip())).

t_loopback() ->
  ?assert(bucinet:is_ip(bucinet:loopback())).


t_active_ips() ->
  ?assert(lists:all(fun bucinet:is_ip/1, bucinet:active_ips())).

t_country() ->
  ?assertEqual({ok, "US", "United States", "America/Los_Angeles"},
               bucinet:country("208.80.152.201")),
  ?assertEqual({error, unknow_ip},
               bucinet:country("127.0.0.1")).

