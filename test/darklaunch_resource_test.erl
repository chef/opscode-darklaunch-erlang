%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Noah Kantrowitz <noah@coderanger.net>
%% @copyright 2011 Opscode, Inc.

-module(darklaunch_resource_test).

-include_lib("eunit/include/eunit.hrl").

-include("../src/darklaunch_resource.hrl").

darklaunch_resource_resource_exists_test_() ->
    {foreachx,
     fun({Method, Feature, Org}) ->
        meck:new(wrq),
        meck:expect(wrq, method, fun(_Req) -> Method end),
        meck:expect(wrq, path_info, fun(feature, _Req) -> Feature; (org, _Req) -> Org end),
        {true, fake_request, State} = darklaunch_resource:resource_exists(fake_request, #state{}),
        State
     end,
     fun(_, _) ->
        meck:unload()
     end,
     [
        {
            {'GET', undefined, undefined},
            fun(_, #state{action=Action, feature=Feature, org=Org}) ->
                [
                    ?_assertEqual(Action, is_enabled),
                    ?_assertEqual(Feature, undefined),
                    ?_assertEqual(Org, undefined)
                ]
            end
        },
        {
            {'GET', "feature1", undefined},
            fun(_, #state{action=Action, feature=Feature, org=Org}) ->
                [
                    ?_assertEqual(Action, is_enabled),
                    ?_assertEqual(Feature, <<"feature1">>),
                    ?_assertEqual(Org, undefined)
                ]
            end
        },
        {
            {'GET', "feature1", "clownco"},
            fun(_, #state{action=Action, feature=Feature, org=Org}) ->
                [
                    ?_assertEqual(Action, is_enabled),
                    ?_assertEqual(Feature, <<"feature1">>),
                    ?_assertEqual(Org, "clownco")
                ]
            end
        },
        {
            {'POST', undefined, undefined},
            fun(_, #state{action=Action, feature=Feature, org=Org}) ->
                [
                    ?_assertEqual(Action, set_enabled),
                    ?_assertEqual(Feature, undefined),
                    ?_assertEqual(Org, undefined)
                ]
            end
        },
        {
            {'POST', "feature1", undefined},
            fun(_, #state{action=Action, feature=Feature, org=Org}) ->
                [
                    ?_assertEqual(Action, set_enabled),
                    ?_assertEqual(Feature, <<"feature1">>),
                    ?_assertEqual(Org, undefined)
                ]
            end
        },
        {
            {'POST', "feature1", "clownco"},
            fun(_, #state{action=Action, feature=Feature, org=Org}) ->
                [
                    ?_assertEqual(Action, set_enabled),
                    ?_assertEqual(Feature, <<"feature1">>),
                    ?_assertEqual(Org, "clownco")
                ]
            end
        }
     ]
    }.

assert_to_json_equal(Action, Feature, Org, Val) ->
    State = #state{action=Action, feature=Feature, org=Org},
    {Out, fake_request, _OutState} = darklaunch_resource:to_json(fake_request, State),
    {Keys} = ejson:decode(Out),
    ?assertEqual({<<"ok">>, true}, lists:keyfind(<<"ok">>, 1, Keys)),
    ?assertEqual({<<"enabled">>, Val}, lists:keyfind(<<"enabled">>, 1, Keys)).

-define(_assertToJsonEqual(Action, Feature, Org, Val), ?_test(assert_to_json_equal(Action, Feature, Org, Val))).

darklaunch_resource_to_json_test_() ->
    {foreachx,
     fun(MockData) ->
        MockData1 = lists:map(
        fun({Feature, Val}) when is_boolean(Val) ->
            {atom_to_binary(Feature, utf8), Val};
        ({Feature, Org}) ->
            {{atom_to_binary(Feature, utf8), atom_to_list(Org)}, true}
        end, MockData),
        meck:new(darklaunch),
        meck:new(wrq),
        meck:expect(darklaunch, is_enabled, fun(Feature) -> case lists:keyfind(Feature, 1, MockData1) of {_, Val} -> Val; false -> false end end),
        meck:expect(darklaunch, is_enabled,
            fun(Feature, Org) ->
                case lists:keyfind({Feature, Org}, 1, MockData1) of
                    {_, Val} ->
                        Val;
                    false ->
                        darklaunch:is_enabled(Feature)
                end
            end),
        meck:expect(darklaunch, set_enabled, 2, ok),
        meck:expect(darklaunch, set_enabled, 3, ok)
     end,
     fun(_, _) ->
        meck:unload()
     end,
     [
        {   %% Make sure my mocking code works as expected
            [{feature1, true}, {feature1, clownco}],
            fun(_, _) ->
                [
                    ?_assert(darklaunch:is_enabled(<<"feature1">>)),
                    ?_assert(darklaunch:is_enabled(<<"feature1">>, "clownco")),
                    ?_assert(darklaunch:is_enabled(<<"feature1">>, "other")),
                    ?_assertNot(darklaunch:is_enabled(<<"feature2">>)),
                    ?_assertNot(darklaunch:is_enabled(<<"feature2">>, "clownco"))
                ]
            end
        },
        {
            [{feature1, true}],
            fun(_, _) ->
                [
                    ?_assertToJsonEqual(is_enabled, <<"feature1">>, undefined, true),
                    ?_assertToJsonEqual(is_enabled, <<"feature2">>, undefined, false),
                    ?_assertToJsonEqual(is_enabled, <<"feature1">>, "clownco", true),
                    ?_assertToJsonEqual(is_enabled, <<"feature2">>, "clownco", false)
                ]
            end
        },
        {
            [],
            fun(_, _) ->
                {?LINE, fun() ->
                    meck:expect(wrq, req_body, fun(fake_request) -> <<"{\"enabled\": true}">> end),
                    assert_to_json_equal(set_enabled, <<"feature1">>, undefined, true),
                    ?assert(meck:called(darklaunch, set_enabled, [<<"feature1">>, true]))
                end}
            end
        },
        {
            [],
            fun(_, _) ->
                {?LINE, fun() ->
                    meck:expect(wrq, req_body, fun(fake_request) -> <<"{\"enabled\": true}">> end),
                    assert_to_json_equal(set_enabled, <<"feature1">>, "clownco", true),
                    ?assert(meck:called(darklaunch, set_enabled, [<<"feature1">>, "clownco", true])),
                    ?assertNot(meck:called(darklaunch, set_enabled, [<<"feature1">>, true]))
                end}
            end
        },
        {
            [],
            fun(_, _) ->
                {?LINE, fun() ->
                    meck:expect(wrq, req_body, fun(fake_request) -> <<"{\"enabled\": false}">> end),
                    assert_to_json_equal(set_enabled, <<"feature1">>, undefined, false),
                    ?assert(meck:called(darklaunch, set_enabled, [<<"feature1">>, false]))
                end}
            end
        },
        {
            [],
            fun(_, _) ->
                {?LINE, fun() ->
                    meck:expect(wrq, req_body, fun(fake_request) -> <<"{\"enabled\": false}">> end),
                    assert_to_json_equal(set_enabled, <<"feature1">>, "clownco", false),
                    ?assert(meck:called(darklaunch, set_enabled, [<<"feature1">>, "clownco", false])),
                    ?assertNot(meck:called(darklaunch, set_enabled, [<<"feature1">>, false]))
                end}
            end
        },
        {
            [],
            fun(_, _) ->
                {?LINE, fun() ->
                    meck:expect(wrq, req_body, fun(fake_request) -> fake_body end),
                    meck:expect(darklaunch, from_json, fun(fake_body) -> ok end),
                    {Out, fake_request, _OutState} = darklaunch_resource:to_json(fake_request,  #state{action=set_enabled}),
                    {Keys} = ejson:decode(Out),
                    ?assertEqual({<<"ok">>, true}, lists:keyfind(<<"ok">>, 1, Keys)),
                    ?assert(meck:called(darklaunch, from_json, [fake_body]))
                end}
            end
        }
     ]
    }.
