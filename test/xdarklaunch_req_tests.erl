%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @copyright 2013 Opscode, Inc.

-module(xdarklaunch_req_tests).

-include_lib("eunit/include/eunit.hrl").

fake_darklaunch(<<"foo">>) ->
    true;
fake_darklaunch(<<"bar">>) ->
    false;
fake_darklaunch(X) ->
    ?debugVal(X),
    true.

parse_header_int_test_() ->
    {foreach,
     fun() ->
             meck:new(darklaunch),
             meck:expect(darklaunch, is_enabled, fun fake_darklaunch/1),
             ok
     end,
     fun(_) ->
             meck:unload(),
             ok
     end,
     [{"Parse a simple header",
       fun() ->
               ?assertEqual(true, fake_darklaunch(<<"foo">>)),
               ?assertEqual(false, fake_darklaunch(<<"bar">>)),
               Dl = xdarklaunch_req:parse_header(fun(_) -> <<"foo=0">> end),
               ?assertEqual(false, xdarklaunch_req:is_enabled(<<"foo">>, Dl))
       end},
      {"Parse a multivalue header",
       fun() ->
               Dl = xdarklaunch_req:parse_header(fun(_) -> <<"foo=0;bar=1">> end),
               ?assertEqual(false, xdarklaunch_req:is_enabled(<<"foo">>, Dl)),
               ?assertEqual(true, xdarklaunch_req:is_enabled(<<"bar">>, Dl))
       end},
      {"Parse a header and ask for a missing value",
       fun() ->
               Dl = xdarklaunch_req:parse_header(fun(_) -> <<"foo=0;bar=1">> end),
               ?assertEqual(false, xdarklaunch_req:is_enabled(<<"foo">>, Dl)),
               ?assertThrow({darklaunch_missing_key, <<"baz">>}, xdarklaunch_req:is_enabled(<<"baz">>, Dl))
       end},
      {"Parse an empty header",
       fun() ->
               Dl = xdarklaunch_req:parse_header(fun(_) -> <<>> end),
               ?assertEqual(no_header, Dl)
       end},
      {"Parse an empty header and query it",
       fun() ->
               Dl = xdarklaunch_req:parse_header(fun(_) -> <<>> end),
               ?assertEqual(no_header, Dl),
               ?assertEqual(true, catch xdarklaunch_req:is_enabled(<<"foo">>, Dl)),
               ?assertEqual(false, catch xdarklaunch_req:is_enabled(<<"bar">>, Dl))
       end},
      {"Parse an undefined header",
       fun() ->
               Dl = xdarklaunch_req:parse_header(fun(_) -> undefined end),
               ?assertEqual(true, xdarklaunch_req:is_enabled(<<"foo">>, Dl)),
               ?assertEqual(false, xdarklaunch_req:is_enabled(<<"bar">>, Dl))
       end},
      {"Parse a missing header",
       fun() ->
               Dl = no_header,
               ?assertEqual(true, xdarklaunch_req:is_enabled(<<"foo">>, Dl)),
               ?assertEqual(false, xdarklaunch_req:is_enabled(<<"bar">>, Dl))
       end}
      ]}.
