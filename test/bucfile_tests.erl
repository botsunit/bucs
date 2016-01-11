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
  ?assertEqual(false, bucfile:match("a/b/c/x", "*/b/*")),
  ?assertEqual(false, bucfile:match("a/b/c/x", "**/b/*")),
  ?assertEqual(false, bucfile:match("a/b/c", "b")),
  ?assert(bucfile:match("a/.b/c", "**/.b/**")),
  ?assertEqual(false, bucfile:match("a/xb/c", "**/.b/**")).

