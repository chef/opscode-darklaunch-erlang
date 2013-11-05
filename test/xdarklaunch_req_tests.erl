%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @copyright 2013 Opscode, Inc.

-module(xdarklaunch_req_tests).

-include_lib("eunit/include/eunit.hrl").

mk_dl(V) ->
    xdarklaunch_req:parse_header(fun("X-Ops-Darklaunch") -> V end).

is_enabled_test_() ->
    [{"Parse a simple header",
      fun() ->
              Dl = mk_dl(<<"foo=0">>),
              ?assertEqual(false, xdarklaunch_req:is_enabled(<<"foo">>, Dl))
      end},
     {"Parse a multivalue header",
      fun() ->
              Dl = mk_dl(<<"foo=0;bar=1">>),
              ?assertEqual(false, xdarklaunch_req:is_enabled(<<"foo">>, Dl)),
              ?assertEqual(true, xdarklaunch_req:is_enabled(<<"bar">>, Dl))
      end},
     {"Parse a header and confirm that missing values are false",
      fun() ->
              Dl = mk_dl(<<"foo=0;bar=1">>),
              ?assertEqual(false, xdarklaunch_req:is_enabled(<<"no-such-key">>, Dl))
      end},
     {"Parse an empty header and query it",
      fun() ->
              Dl = mk_dl(<<>>),
              ?assertEqual(no_header, Dl),
              ?assertEqual(false, xdarklaunch_req:is_enabled(<<"foo">>, Dl))
      end},
     {"Parse an undefined header",
      fun() ->
              Dl = mk_dl(undefined),
              ?assertEqual(false, xdarklaunch_req:is_enabled(<<"foo">>, Dl))
      end}
    ].

is_enabled_with_default_value_test_() ->
    Dl = mk_dl(<<"a=1;b=0">>),
    [{"default value is ignored if feature is present",
      fun() ->
              ?assertEqual(true, xdarklaunch_req:is_enabled(<<"a">>, Dl, false)),
              ?assertEqual(false, xdarklaunch_req:is_enabled(<<"b">>, Dl, true))
      end},
     {"default value is used if feature is missing with header",
      fun() ->
              ?assertEqual(true, xdarklaunch_req:is_enabled(<<"c">>, Dl, true)),
              ?assertEqual(false, xdarklaunch_req:is_enabled(<<"c">>, Dl, false)),
              ?assertEqual(anything, xdarklaunch_req:is_enabled(<<"c">>, Dl, anything))
      end},
     {"default value is used if feature is missing with no header",
      fun() ->
              NH = mk_dl(<<>>),
              ?assertEqual(true, xdarklaunch_req:is_enabled(<<"c">>, NH, true)),
              ?assertEqual(false, xdarklaunch_req:is_enabled(<<"c">>, NH, false)),
              ?assertEqual(anything, xdarklaunch_req:is_enabled(<<"c">>, NH, anything))
      end}].
