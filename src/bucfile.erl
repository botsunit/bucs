-module(bucfile).

-export([
         expand_path/1,
         normalize_path/1,
         user_home/0,
         make_dir/1,
         remove_recursive/1,
         copy/2,
         copy/3,
         copyfile/2,
         copyfile/3,
         relative_from/2,
         realpath/1,
         wildcard/2,
         wildcard/3,
         match/2,
         match/3,
         is_executable/1,
         is_executable/2,
         is_symlink/1,
         is_broken/1
        ]).
-include_lib("kernel/include/file.hrl").

%% @doc
%% Expand the given path
%%
%% Example:
%%
%% <pre lang="erlang">
%% "/home/user" = bucfile:expand_path("~").
%% &lt;&lt;"/home/user"&gt;&gt; = bucfile:expand_path(&lt;&lt;"~"&gt;&gt;).
%% </pre>
%% @end
-spec expand_path(string() | binary()) -> binary() | list().
expand_path(Path) when is_binary(Path) ->
  do_as_list(?MODULE, expand_path, Path);
expand_path(Path) when is_list(Path) ->
  normalize_path(filename:absname(expand_home(Path))).

%% @doc
%% Normalize the given path
%%
%% Example:
%%
%% <pre lang="erlang">
%% "/" = bucfile:normalize_path("/toto/tutu/../../../../../..").
%% &lt;&lt;"/"&gt;&gt; = bucfile:normalize_path(&lt;&lt;"/toto/tutu/../../../../../.."&gt;&gt;).
%% "/toto/titi" = bucfile:normalize_path("/toto/tata/../titi").
%% </pre>
%% @end
-spec normalize_path(string() | binary()) -> string() | binary().
normalize_path(Path) when is_binary(Path) ->
  do_as_list(?MODULE, normalize_path, Path);
normalize_path(Path) when is_list(Path) ->
  normalize_path(filename:split(Path), []).
normalize_path([".."|T], []) ->
  normalize_path(T, []);
normalize_path([".."|T], [_|Acc]) ->
  normalize_path(T, Acc);
normalize_path(["."|T], Acc) ->
  normalize_path(T, Acc);
normalize_path([H|T], Acc) ->
  normalize_path(T, [H|Acc]);
normalize_path([], Acc) ->
  case length(Acc) of
    0 -> "/";
    _ -> filename:join(lists:reverse(Acc))
  end.

%% @doc
%% Return the HOME directory
%%
%% Example:
%%
%% <pre lang="erlang">
%% "/home/user" = bucfile:user_home().
%% </pre>
%% @end
-spec user_home() -> string().
user_home() ->
  case os:type() of
    {win32, _} -> get_windows_home();
    _ -> get_unix_home()
  end.

%% @doc
%% Create the given directory if it not exist
%% @end
make_dir(Path) when is_binary(Path) ->
  do_as_list(?MODULE, make_dir, Path);
make_dir(Path) ->
  filelib:ensure_dir(filename:join([Path, "."])).

%% @doc
%% Remove, recursively the given path
%% @end
remove_recursive(Path) ->
  case filelib:is_dir(Path) of
    false ->
      file:delete(Path);
    true ->
      lists:foreach(fun remove_recursive/1, sub_files(Path)),
      file:del_dir(Path)
  end.

%% @equiv copy(Source, Destination, [preserve_file_info, recursive])
copy(Source, Destination) ->
  do_copy(Source, Destination, [preserve_file_info, recursive]).

%% @doc
%% Copy a <tt>Source</tt> to a <tt>Destination</tt>
%%
%% Available options:
%% <ul>
%%   <li><tt>recursive</tt></li>
%%   <li><tt>{exclude, [file:filename()]}</tt></li>
%%   <li><tt>{only, [file:filename()]}</tt></li>
%%   <li><tt>preserve_file_info</tt> (default)</li>
%%   <li><tt>default_file_info</tt></li>
%%   <li><tt>{directory_mode, integer()}</tt></li>
%%   <li><tt>{regular_file_mode, integer()}</tt></li>
%%   <li><tt>{executable_file_mode, integer()}</tt></li>
%% </ul>
%% @end
copy(Source, Destination, Options) ->
  case lists:foldl(fun(Option, Acc) ->
                       case lists:member(Option, Options) of
                         true -> true;
                         false ->
                           case lists:keyfind(Option, 1, Options) of
                             false -> Acc;
                             _Tuple -> true
                           end
                       end
                   end, false, [default_file_info, preserve_file_info, regular_file_mode, executable_file_mode, directory_mode]) of
    true -> do_copy(Source, Destination, Options);
    false -> do_copy(Source, Destination, [preserve_file_info|Options])
  end.

do_copy(Source, Destination, Options) ->
  case lists:member(recursive, Options) of
    true ->
      Base = filename:basename(Source),
      Dest = filename:join(Destination, Base),
      case filelib:is_dir(Source) of
        false ->
          case filelib:is_dir(Destination) of
            true ->
              copyfile(Source, Dest, Options);
            false ->
              copyfile(Source, Destination, Options)
          end;
        true ->
          _ = build_dir(Source, Dest, Options),
          SubFiles = sub_files(Source),
          SubFiles1 = case lists:keyfind(exclude, 1, Options) of
                        false -> SubFiles;
                        {exclude, ExcludedFiles} ->
                          buclists:delete_if(
                            fun(File) ->
                                lists:any(
                                  fun(Exclude) ->
                                      string:str(expand_path(File), Exclude) =/= 0
                                  end, ExcludedFiles)
                            end, SubFiles)
                      end,
          SubFiles2 = case lists:keyfind(only, 1, Options) of
                        false ->
                          SubFiles1;
                        {only, OnlyFiles} ->
                          buclists:delete_if(
                            fun(File) ->
                                lists:all(
                                  fun(Only) ->
                                      string:str(expand_path(File), Only) =:= 0
                                  end, OnlyFiles)
                            end, SubFiles1)
                      end,
          lists:foreach(fun(File) ->
                            do_copy(File, Dest, Options)
                        end, SubFiles2)
      end;
    false ->
      copyfile(Source, Destination, Options)
  end.

build_dir(Source, Destination, Options) ->
  case make_dir(Destination) of
    ok ->
      lists:foreach(fun(Option) ->
                        case lists:member(Option, Options) of
                          true -> change_file_mode(Source, Destination, Option);
                          false ->
                            case lists:keyfind(Option, 1, Options) of
                              false -> ok;
                              Tuple -> change_file_mode(Source, Destination, Tuple)
                            end
                        end
                    end, [default_file_info, preserve_file_info, directory_mode, executable_file_mode]);
    {error, Reason} ->
      error(Reason)
  end.

%% @equiv copyfile(Source, Destination, [preserve_file_info])
copyfile(Source, Destination) ->
  copyfile(Source, Destination, [preserve_file_info]).
%% @doc
%% Copy file <tt>Source</tt> to a <tt>Destination</tt>
%%
%% Available options:
%% <ul>
%%   <li><tt>preserve_file_info</tt> (default)</li>
%%   <li><tt>default_file_info</tt></li>
%%   <li><tt>{directory_mode, integer()}</tt></li>
%%   <li><tt>{regular_file_mode, integer()}</tt></li>
%%   <li><tt>{executable_file_mode, integer()}</tt></li>
%% </ul>
%% @end
copyfile(Source, Destination, Options) ->
  case (is_symlink(Source) andalso (not is_broken(Source))) orelse (not is_symlink(Source)) of
    true ->
      case file:copy(Source, Destination) of
        {ok, _} ->
          lists:foreach(fun(Option) ->
                            case lists:member(Option, Options) of
                              true -> change_file_mode(Source, Destination, Option);
                              false ->
                                case lists:keyfind(Option, 1, Options) of
                                  false -> ok;
                                  Tuple -> change_file_mode(Source, Destination, Tuple)
                                end
                            end
                        end, [default_file_info, preserve_file_info, regular_file_mode, executable_file_mode]);
        {error, Reason} ->
          error(io_lib:format("~p -> ~p : ~p", [Source, Destination, Reason]))
      end;
    false ->
      ok
  end.

change_file_mode(_, _, default_file_info) ->
  ok;
change_file_mode(Source, Destination, preserve_file_info) ->
  case file:read_file_info(Source) of
    {ok, FileInfo} ->
      case file:write_file_info(Destination, FileInfo) of
        ok -> ok;
        {error, Reason} ->
          error(Reason)
      end;
    {error, Reason} ->
      error(Reason)
  end;
change_file_mode(_, Destination, {regular_file_mode, Mode}) ->
  case filelib:is_file(Destination) of
    true ->
      case file:change_mode(Destination, Mode) of
        ok -> ok;
        {error, Reason1} ->
          error(Reason1)
      end;
    false ->
      ok
  end;
change_file_mode(Source, Destination, {executable_file_mode, Mode}) ->
  case filelib:is_file(Destination) and is_executable(Source, [owner, group, other]) of
    true ->
      case file:change_mode(Destination, Mode) of
        ok -> ok;
        {error, Reason1} ->
          error(Reason1)
      end;
    false ->
      ok
  end;
change_file_mode(_, Destination, {directory_mode, Mode}) ->
  case filelib:is_dir(Destination) of
    true ->
      case file:change_mode(Destination, Mode) of
        ok -> ok;
        {error, Reason1} ->
          error(Reason1)
      end;
    false ->
      ok
  end.

% @doc
% Return true if <tt>File</tt> is executable, false otherwise
% @end
is_executable(File) ->
  case file:read_file_info(File) of
    {ok, #file_info{mode = Mode}} ->
      case integer_to_list(Mode, 8) of
        [_, _, _, U, G, O] ->
          #{owner => not((list_to_integer([U]) band 1) == 0),
            group => not((list_to_integer([G]) band 1) == 0),
            other => not((list_to_integer([O]) band 1) == 0)};
        _ ->
          #{owner => false,
            group => false,
            other => false}
      end;
    _ ->
      #{owner => false,
        group => false,
        other => false}
  end.
% @doc
% Return true if <tt>File</tt> is executable for <tt>Who</tt>, false otherwise
% @end
-spec is_executable(File :: file:filename(), Who :: owner | group | other | list()) -> true | false.
is_executable(File, Who) when Who == owner;
                              Who == group;
                              Who == other ->
  maps:get(Who, is_executable(File));
is_executable(File, Who) when is_list(Who) ->
  lists:foldl(fun(W, Acc) ->
                  Acc or is_executable(File, W)
              end, false, Who).

%% @doc
%% Return the given <tt>FilePath</tt> relatively to the <tt>FromPath</tt>.
%% @end
relative_from(FilePath, FromPath) ->
  case get_real_path(FilePath) of
    {ok, FilePath1} ->
      case get_real_path(FromPath) of
        {ok, FromPath1} ->
          realpath(
            filename:join(
              relative_from1(
                filename:split(FilePath1),
                filename:split(FromPath1))));
        E -> E
      end;
    E -> E
  end.

%% @doc
%% Return the realpath of the given path
%% @end
realpath(Path) ->
  filename:join(
    realpath(
      filename:split(Path),
      []
     )
   ).

%% @equiv wildcard(Path, Exclude, [])
wildcard(Path, Exclude) ->
  wildcard(Path, Exclude, []).

%% @doc
%% Same as <tt>filelib:wildcard/1</tt> but where expressions listed in <tt>Exclude</tt> are excluded.
%%
%% The exclude string looks like a wildcard string.
%%
%% <tt>Options:</tt>
%% <ul>
%% <li><tt>expand_path</tt> : the <tt>Path</tt> wil be expanded using <tt>bucfile:expand_path/1</tt></li>
%% <li><tt>{cd, From}</tt> : the <tt>Path</tt> will be prefixed with <tt>From</tt></li>
%% </ul>
%% @end
wildcard(Path, Exclude, Options) ->
  buclists:delete_if(
    fun(P) ->
        lists:any(
          fun(E) ->
              match(P, E, Options)
          end, Exclude)
    end, filelib:wildcard(Path)).

%% @equiv match(Path, Exp, [])
match(Path, Exp) ->
  match(Path, Exp, []).

%% @doc
%% Return true if the <tt>Path</tt> match the <tt>Expression</tt>
%%
%% The exclude string looks like a wildcard string.
%%
%% <tt>Options:</tt>
%% <ul>
%% <li><tt>expand_path</tt> : the <tt>Path</tt> wil be expanded using <tt>bucfile:expand_path/1</tt></li>
%% <li><tt>{cd, From}</tt> : the <tt>Path</tt> will be prefixed with <tt>From</tt></li>
%% </ul>
%%
%% Example:
%%
%% <pre>
%% bucfile:match("a/b/c", "**/b/**").
%% % => true
%% bucfile:match("a/b/c", "**/a/**").
%% % => false
%% bucfile:match("/a/b/c", "**/a/**").
%% % => true
%% bucfile:match("a/b/c", "**/a/**", [expand_path]).
%% % => true
%% bucfile:match("a/b/c", "**/a/**", [{cd, "/tmp"}]).
%% % => true
%% bucfile:match("a/b/c", "**/tmp/**", [{cd, "/tmp"}]).
%% % => true
%% bucfile:match("a/b/c", "**/tmp/**", [{cd, "tmp"}]).
%% % => false
%% bucfile:match("a/b/c", "**/tmp/**", [expand_path, {cd, "tmp"}]).
%% % => true
%% </pre>
%% @end
match(Path, Expression, Options) ->
  Path1 = case lists:keyfind(cd, 1, Options) of
            {cd, CD} -> filename:join([CD, Path]);
            _ -> Path
          end,
  Path2 = case lists:member(expand_path, Options) of
            true -> expand_path(Path1);
            false -> Path1
          end,
  Expression0 = bucstring:gsub(Expression, ".", "\\."),
  Expression1 = bucstring:gsub(Expression0, "?", "."),
  Expression2 = bucstring:gsub(Expression1, "*", "[^/]*"),
  Expression3 = bucstring:gsub(Expression2, "[^/]*[^/]*", ".*"),
  Expression4 = "^" ++ Expression3 ++ "$",
  {Expression5, _} = lists:foldl(fun
                         (${, {Acc, none}) ->
                           {[$(|Acc], in};
                         ($}, {Acc, in}) ->
                           {[$)|Acc], none};
                         ($,, {Acc, in}) ->
                           {[$||Acc], in};
                         (32, {Acc, in}) ->
                           {Acc, in};
                         (C, {Acc, T}) ->
                           {[C|Acc], T}
                       end, {"", none}, Expression4),
  Expression6 = lists:reverse(Expression5),
  case re:run(Path2, Expression6) of
    nomatch -> false;
    _ -> true
  end.

%% @doc
%% Return true if <tt>Path</tt> is a symlink, false otherwise
%% @end
is_symlink(Path) ->
  case file:read_link_info(Path) of
    {ok, #file_info{type = symlink}} ->
      true;
    _ ->
      false
  end.

%% @doc
%% @end
is_broken(Path) ->
  case is_symlink(Path) of
    true ->
      case file:read_link_info(Path) of
        {ok, #file_info{access = Access}} when Access =/= none, Access =/= undefined ->
          false;
        _ ->
          true
      end;
    false ->
      false
  end.

% private

get_real_path(Path) ->
  case filename:split(Path) of
    ["/"|_] -> {ok, Path};
    FilePath3 ->
      case file:get_cwd() of
        {ok, Dir} -> {ok, realpath(filename:join(filename:split(Dir) ++ FilePath3))};
        E -> E
      end
  end.

relative_from1([C|File], [C|From]) ->
  relative_from1(File, From);
relative_from1(File, From) ->
  [".." || X <- From, X =/= "/"] ++ File.

realpath([], Result) ->
  Result;
realpath([Current|List], Result) when Current =:= "..", length(Result) > 0 ->
  case lists:reverse(Result) of
    [".."|_] ->
      realpath(List, Result ++ [Current]);
    _ ->
      case re:run(Result, "^.*/$") of
        {match, _} ->
          realpath(List, Result);
        nomatch ->
          realpath(List, lists:reverse(tl(lists:reverse(Result))))
      end
  end;
realpath([Current|List], Result) when Current =:= "." ->
  realpath(List, Result);
realpath([Current|List], Result) ->
  realpath(List, Result ++ [Current]).

sub_files(From) ->
  {ok, SubFiles} = file:list_dir(From),
  [filename:join(From, SubFile) || SubFile <- SubFiles].

expand_home([$~|Rest]) ->
  user_home() ++ Rest;
expand_home(Path) -> Path.

get_unix_home() ->
  os:getenv("HOME").

get_windows_home() ->
  filename:absname(
    case os:getenv("USERPROFILE") of
      false ->
        get_windows_home(os:getenv("HOMEDRIVE"));
      Path -> Path
    end
   ).
get_windows_home(false) -> false;
get_windows_home(HomeDrive) -> get_windows_home(HomeDrive, os:getenv("HOMEPATH")).
get_windows_home(_, false) -> false;
get_windows_home(HomeDrive, HomePath) -> HomeDrive ++ HomePath.

do_as_list(Module, Function, Binary) when is_atom(Module), is_atom(Function), is_binary(Binary) ->
  list_to_binary(erlang:apply(Module, Function, [binary_to_list(Binary)]));
do_as_list(Module, Function, Binaries) when is_atom(Module), is_atom(Function), is_list(Binaries) ->
  Lists = lists:map(fun binary_to_list/1, Binaries),
  list_to_binary(erlang:apply(Module, Function, Lists)).
