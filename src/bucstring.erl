-module(bucstring).

-export([sub/3, gsub/3]).

%% @doc
%% Return an new string with the first occurance of Old substitued by New
%%
%% Example:
%% <pre lang="erlang">
%% "HeLlo World" = estring:sub("Hello World", "l", "L").
%% </pre>
%% @end
-spec sub(string(), string(), string()) -> string().
sub(Str, Old, New) ->
  FStr = lists:flatten(Str),
  FOld = lists:flatten(Old),
  FNew = lists:flatten(New),
  Lstr = length(FStr),
  Lold = length(FOld),
  Pos  = string:str(FStr, FOld),
  if
    Pos =:= 0 ->
      FStr;
    true      ->
      LeftPart = string:left(FStr, Pos-1),
      RitePart = string:right(FStr, Lstr-Lold-Pos+1),
      string:concat(string:concat(LeftPart, FNew), RitePart)
  end.

%% @doc
%% Return an new string with the all occurances of Old substitued by New
%%
%% Example:
%% <pre lang="erlang">
%% "HeLLo WorLd" = estring:gsub("Hello World", "l", "L").
%% </pre>
%% @end
-spec gsub(string(), string(), string()) -> string().
gsub(Str, Old, New) ->
  %Acc = sub(Str,Old,New),
  %subst(Acc,Old,New,Str).
  gsub(Str, Old, New, "").

gsub("", _Old, _New, Acc) -> lists:flatten(Acc);
gsub(Str, Old, New, Acc) ->
  case string:str(Str, Old) of
    0 ->
      gsub("", Old, New, Acc ++ Str);
    Pos ->
      Pre = string:left(Str, Pos - 1),
      Rest = string:right(Str, length(Str) - Pos + 1 - length(Old)),
      gsub(Rest, Old, New, Acc ++ Pre ++ New)
  end.
