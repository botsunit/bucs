%% @doc
%% @author Grégoire Lejeune <gregoire.lejeune@botsunit.com>
%% @author Mathias Franck <mathias.franck@botsunit.com>
%% @copyright 2016 BotsUnit
%%
%% Erlang server for generating random strings, usable as file names, tokens,...
%%
%% The implementation avoids problems with "rand-seeding"
%% the Erlang random generator between processes : Using bucrandom:randstr/1
%% guarantees a good random distribution, and low probability of getting
%% the same value at first call, from wherever the function is called.
%% However, it may not be appropriate for cryptographic or 'sensible' purposes.
%% @end
-module(bucrandom).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(RAND, begin
                case code:ensure_loaded(rand) of
                  {module, rand} -> rand;
                  _ -> random
                end
              end).
-define(SEED_STATE, begin
                      case code:ensure_loaded(rand) of
                        {module, rand} -> exs1024;
                        _ -> erlang:system_time(micro_seconds)
                      end
                    end).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([randstr/1]).

% @hidden
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


% @doc
% Returns a random string of a given length,
% that contains only letters (no diacritics) or digits
%
% Param:
%
% * Length, the length of the string to generate
%
% Example:
%
% <pre lang="erlang">
% 1> bucrandom:randstr(12).
% "ZL7YmS5HRQod"
% </pre>
% @end
-spec randstr(Length::integer()) -> string().
randstr(Length) ->
  _ = ensure_started(),
  gen_server:call(?SERVER, {randstr, Length}).

% @hidden
init(Args) ->
  _ = ?RAND:seed(?SEED_STATE),
  {ok, Args}.

% @hidden
handle_call({randstr, Length}, _From, State) ->
  {reply, private_randstr(Length), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Service implementation

-define(CHARS, "azertyuiopqsdfghjklmwxcvbnAZERTYUIOPQSDFGHJKLMWXCVBN1234567890").

private_randstr(Size) ->
  lists:flatten([lists:sublist(?CHARS, ?RAND:uniform(length(?CHARS)), 1) || _ <- lists:seq(1, Size)]).

ensure_started() ->
  case [A || {A, _, _} <- application:which_applications(), A =:= bucs] of
    [] -> application:ensure_all_started(bucs);
    [bucs] -> ok
  end.

