-module(bucfile_tests).

-include_lib("eunit/include/eunit.hrl").

bucfile_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(t_realpath())
      , ?_test(t_realtive_from())
      , ?_test(t_match())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_realpath() ->
  case os:type() of
    win32 ->
      ?assertEqual("/", bucfile:realpath("/../../../../..")),
      ?assertEqual("c:\\toto\\titi", bucfile:realpath("c:\\toto\\titi\\tutu\\tata\\..\\..")),
      ?assertEqual("c:\\toto\\tutu", bucfile:realpath("c:\\toto\\titi\\..\\tutu\\tata\\..")),
      ?assertEqual("c:\\tutu\\tata", bucfile:realpath("c:\\toto\\titi\\..\\..\\tutu\\tata")),
      ?assertEqual("c:\\toto", bucfile:realpath("c:\\..\\toto")),
      ?assertEqual("c:\\", bucfile:realpath("c:\\toto\\..")),
      ?assertEqual("c:\\", bucfile:realpath("c:\\toto\\..\\..")),
      ?assertEqual("c:\\toto", bucfile:realpath("c:\\toto\\.")),
      ?assertEqual("c:\\toto\\titi", bucfile:realpath("c:\\toto\\.\\titi\\.")),
      ?assertEqual("c:\\toto", bucfile:realpath("c:\\..\\toto\\titi\\..\\.\\.\\.")),
      ?assertEqual("c:\\", bucfile:realpath("c:\\..\\..\\..\\..\\.."));
    _ ->
      ?assertEqual("/toto/titi", bucfile:realpath("/toto/titi/tutu/tata/../..")),
      ?assertEqual("/toto/tutu", bucfile:realpath("/toto/titi/../tutu/tata/..")),
      ?assertEqual("/tutu/tata", bucfile:realpath("/toto/titi/../../tutu/tata")),
      ?assertEqual("/toto", bucfile:realpath("/../toto")),
      ?assertEqual("/", bucfile:realpath("/toto/..")),
      ?assertEqual("/", bucfile:realpath("/toto/../..")),
      ?assertEqual("/toto", bucfile:realpath("/toto/.")),
      ?assertEqual("/toto/titi", bucfile:realpath("/toto/./titi/.")),
      ?assertEqual("/toto", bucfile:realpath("/../toto/titi/../././.")),
      ?assertEqual("/", bucfile:realpath("/../../../../.."))
  end,
  ?assertEqual("../titi", bucfile:realpath("../titi")),
  ?assertEqual("../toto", bucfile:realpath("../toto/titi/../././.")),
  ?assertEqual("../../../toto", bucfile:realpath("../../../toto/titi/../././.")).

t_realtive_from() ->
  ?assertEqual("file.txt", bucfile:relative_from("/toto/titi/file.txt", "/toto/titi")),
  ?assertEqual("titi/file.txt" , bucfile:relative_from("/toto/titi/file.txt", "/toto")),
  ?assertEqual("../titi/file.txt" , bucfile:relative_from("/toto/titi/file.txt", "/toto/tutu")),
  ?assertEqual("../titi/file.txt" , bucfile:relative_from("toto/titi/file.txt", "toto/tutu")),
  ?assertEqual("../../toto/titi/file.txt" , bucfile:relative_from("/toto/titi/file.txt", "/tata/tutu")).

t_match() ->
  ?assert(bucfile:match("a/b/c", "**/b/**")),
  ?assert(bucfile:match("a/b/c", "*/b/*")),
  ?assert(bucfile:match("a/b/c", "**/b/*")),
  ?assert(bucfile:match("a/b/c", "*/b/**")),
  ?assert(bucfile:match("a/b/c/x", "**/b/**")),
  ?assert(bucfile:match("a/b/c/x", "*/b/**")),
  ?assert(bucfile:match("a/b/c/x", "*/b/**")),
  ?assertNot(bucfile:match("a/b/c/x", "*/b/*")),
  ?assertNot(bucfile:match("a/b/c/x", "**/b/*")),
  ?assertNot(bucfile:match("a/b/c", "b")),
  ?assert(bucfile:match("a/.b/c", "**/.b/**")),
  ?assert(bucfile:match("a/xb/c", "**/?b/**")),
  ?assertNot(bucfile:match("a/b/c", "**/a/**")),
  ?assert(bucfile:match("/a/b/c", "**/a/**")),
  ?assert(bucfile:match("a/b/c", "**/a/**", [expand_path])),
  ?assert(bucfile:match("a/b/c", "**/a/**", [{cd, "/tmp"}])),
  ?assert(bucfile:match("a/b/c", "**/tmp/**", [{cd, "/tmp"}])),
  ?assertNot(bucfile:match("a/b/c", "**/tmp/**", [{cd, "tmp"}])),
  ?assert(bucfile:match("a/b/c", "**/tmp/**", [expand_path, {cd, "tmp"}])),
  ?assert(bucfile:match("toto", "t?t?")),
  ?assert(bucfile:match("tatu", "t?t?")),
  ?assertNot(bucfile:match("tatu", "tot?")),
  ?assertNot(bucfile:match("tatu", "t?to")),
  ?assert(bucfile:match("tatu", "t[a,e,i,o,u]t[a,e,i,o,u]")),
  ?assertNot(bucfile:match("tatu", "t[b,c,d,f,g]t[a,e,i,o,u]")),
  ?assertNot(bucfile:match("tatu", "t[a,e,i,o,u]t[b,c,d,f,g]")),
  ?assert(bucfile:match("hello.erl", "hello.{erl,hrl}")),
  ?assertNot(bucfile:match("hello.cpp", "hello.{erl,hrl}")),
  ?assert(bucfile:match("hello.erl", "hello?erl")),
  ?assert(bucfile:match("hello.erl", "he??o.erl")),
  ?assertNot(bucfile:match("helllo.erl", "he??o.erl")),
  ?assert(bucfile:match("hello.erl", "hello.erl")),
  ?assertNot(bucfile:match("hello-erl", "hello.erl")).

