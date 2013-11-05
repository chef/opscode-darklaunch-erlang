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

parse_header_test_() ->
    [{"true false 0 1",
      fun() ->
              [ begin
                    DL = xdarklaunch_req:parse_header(fun(_) -> H end ),
                    ?assertEqual(true, xdarklaunch_req:is_enabled(<<"a">>, DL)),
                    ?assertEqual(false, xdarklaunch_req:is_enabled(<<"b">>, DL))
                end || H <- [<<"a=1;b=0">>, <<"a=true;b=false">>] ]
      end},
     {"for duplicate keys, last occurance is used",
      fun() ->
              DL = xdarklaunch_req:parse_header(fun(_) -> <<"a=1;b=0;a=0;a=0;a=1;a=0">> end ),
              ?assertEqual(false, xdarklaunch_req:is_enabled(<<"a">>, DL))
      end},
     {"things that aren't true or 1 are false",
      fun() ->
              FalseThings = [<<"a=2">>,
                             <<"a=anything">>,
                             <<"a=-1">>,
                             <<"a=null">>],
              [ begin
                    DL = xdarklaunch_req:parse_header(fun(_) -> H end ),
                    ?assertEqual(false, xdarklaunch_req:is_enabled(<<"a">>, DL))
                end || H <- FalseThings ]
      end}
    ].

get_header_test() ->
    Header = <<"a = 1; b = 2">>,
    DL = xdarklaunch_req:parse_header(fun(_) -> Header end ),
    ?assertEqual({"X-Ops-Darklaunch", Header}, xdarklaunch_req:get_header(DL)).

get_proplist_test() ->
    Header = <<"a = 1; b = 2;c=false;d=true">>,
    DL = xdarklaunch_req:parse_header(fun(_) -> Header end ),
    PL = xdarklaunch_req:get_proplist(DL),
    Expect = [{<<"d">>, true},
              {<<"c">>, false},
              {<<"b">>, <<"2">>},
              {<<"a">>, true}],
    [ ?assertEqual(E, proplists:get_value(K, PL))
      || {K, E} <- Expect ].

get_no_header_test_() ->
    DL = xdarklaunch_req:parse_header(fun(_) -> <<>> end ),
    [?_assertEqual([], xdarklaunch_req:get_proplist(DL)),
     ?_assertEqual({"X-Ops-Darklaunch", ""}, xdarklaunch_req:get_header(DL))].
