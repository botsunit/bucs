-module(buccode).

-export([priv_dir/1]).

% @doc
% Returns the path to the priv directory in an application.
% @end
-spec priv_dir(atom()) -> file:filename() | error.
priv_dir(App) ->
  case code:priv_dir(App) of
    {error, bad_name} ->
      case code:which(App) of
        non_existing ->
          error;
        AppFilename ->
          bucs:pipecall([
                         {fun filename:dirname/1, [AppFilename]},
                         fun filename:dirname/1,
                         {fun filename:join/2, ["priv"]}
                        ])
      end;
    Path ->
      Path
  end.

