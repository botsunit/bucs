% @hidden
-module(bucs_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type, Shutdown), {I, {I, start_link, []}, permanent, Shutdown, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {
     {one_for_one, 1, 5},
     [
      ?CHILD(bucrandom, worker, 5000)
     ]
    }
  }.

