-module(darklaunch_test).

-include_lib("eunit/include/eunit.hrl").

scanenv([]) ->
    throw(not_found);
scanenv([H|T]) ->
    case os:getenv(H) of
        false ->
            scanenv(T);
        Value ->
            Value
    end.

tempfile(Stem) ->
    Tempdir = scanenv(["TMPDIR", "TEMP", "TMP"]),
    Basename = filename:join([Tempdir, Stem]),
    Filename = test_server:temp_name(Basename),
    case file:open(Filename, [write, exclusive, binary]) of
        {error, eexists} ->
            tempfile(Stem);
        {ok, IoDevice} ->
            {IoDevice, Filename}
    end.

canonical_org_features({Key, Value}) when is_list(Value) ->
    {Key, lists:sort(Value)};
canonical_org_features(Any) ->
    Any.

canonical_features(Bin) ->
    {Keys} = ejson:decode(Bin),
    SortedKeys = lists:sort(lists:map(fun canonical_org_features/1, Keys)),
    ejson:encode({SortedKeys}).


darklaunch_test_() ->
    {foreachx,
     fun(Json) ->
        {TempFile, TempFileName} = tempfile("darklaunch"),
        file:write(TempFile, Json),
        application:set_env(darklaunch, config, TempFileName),
        application:set_env(darklaunch, reload_time, 1000),
        darklaunch:start_link(),
        {TempFile, TempFileName}
     end,
     fun(_Json, {_TempFile, TempFileName}) ->
        meck:unload(),
        darklaunch:stop_link(),
        ok = file:delete(TempFileName)
     end,
     [
        {
            <<"{\"feature1\": true}">>,
            fun(Json, _) ->
                [
                    ?_assert(darklaunch:is_enabled(<<"feature1">>)),
                    ?_assert(darklaunch:is_enabled(<<"feature1">>, "clownco")),
                    ?_assertNot(darklaunch:is_enabled(<<"feature2">>)),
                    ?_assertNot(darklaunch:is_enabled(<<"feature2">>, "clownco")),
                    ?_assertEqual(canonical_features(darklaunch:to_json()), canonical_features(Json))
                ]
            end
        },
        {
            <<"{\"feature1\": [\"clownco\"]}">>,
            fun(Json, _) ->
                [
                    ?_assertNot(darklaunch:is_enabled(<<"feature1">>)),
                    ?_assert(darklaunch:is_enabled(<<"feature1">>, "clownco")),
                    ?_assertNot(darklaunch:is_enabled(<<"feature1">>, "someoneelse")),
                    ?_assertEqual(canonical_features(darklaunch:to_json()), canonical_features(Json))
                ]
            end
        },
        {
            <<"{\"feature1\": [\"clownco\", \"local\"], \"feature2\": true}">>,
            fun(Json, _) ->
                [
                    ?_assertNot(darklaunch:is_enabled(<<"feature1">>)),
                    ?_assert(darklaunch:is_enabled(<<"feature1">>, "clownco")),
                    ?_assert(darklaunch:is_enabled(<<"feature1">>, "local")),
                    ?_assertNot(darklaunch:is_enabled(<<"feature1">>, "someoneelse")),
                    ?_assert(darklaunch:is_enabled(<<"feature2">>)),
                    ?_assert(darklaunch:is_enabled(<<"feature2">>, "clownco")),
                    ?_assert(darklaunch:is_enabled(<<"feature2">>, "local")),
                    ?_assert(darklaunch:is_enabled(<<"feature2">>, "someoneelse")),
                    ?_assertEqual(canonical_features(darklaunch:to_json()), canonical_features(Json))
                ]
            end
        },
        {
            <<"{\"feature1\": true}">>,
            fun(_, {TempFile, _TempFileName}) ->
                fun() ->
                    ?assert(darklaunch:is_enabled(<<"feature1">>)),
                    ?assertNot(darklaunch:is_enabled(<<"feature2">>)),
                    test_server:sleep(1000), %% Sleep for one second to make sure the timestamp differs
                    file:position(TempFile, 0),
                    file:write(TempFile, <<"{\"feature2\": true}">>),
                    ok = darklaunch:reload_features(),
                    ?assertNot(darklaunch:is_enabled(<<"feature1">>)),
                    ?assert(darklaunch:is_enabled(<<"feature2">>))
                end
            end
        },
        {
            <<"{\"feature1\": true}">>,
            fun(_, {_TempFile, TempFileName}) ->
                fun() ->
                    ?assert(darklaunch:is_enabled(<<"feature1">>)),
                    ?assertNot(darklaunch:is_enabled(<<"feature2">>)),
                    darklaunch:from_json(<<"{\"feature2\": true}">>),
                    ?assertNot(darklaunch:is_enabled(<<"feature1">>)),
                    ?assert(darklaunch:is_enabled(<<"feature2">>)),
                    {ok, Bin} = file:read_file(TempFileName),
                    ?assertEqual(Bin, <<"{\"feature2\": true}">>)
                end
            end
        },
        {
            <<"{\"feature1\": true}">>,
            fun(_, {_TempFile, TempFileName}) ->
                fun() ->
                    ?assert(darklaunch:is_enabled(<<"feature1">>)),
                    ?assertNot(darklaunch:is_enabled(<<"feature2">>)),
                    darklaunch:from_json(<<"{\"feature2\": true}">>),
                    ?assertNot(darklaunch:is_enabled(<<"feature1">>)),
                    ?assert(darklaunch:is_enabled(<<"feature2">>)),
                    {ok, Bin} = file:read_file(TempFileName),
                    ?assertEqual(Bin, <<"{\"feature2\": true}">>)
                end
            end
        },
        {
            <<"{\"feature2\": [\"clownco\"], \"feature3\": [\"local\"]}">>,
            fun(_, {_TempFile, _TempFileName}) ->
                fun() ->
                    ?assert(darklaunch:is_enabled(<<"feature2">>, "clownco")),
                    ?assert(darklaunch:is_enabled(<<"feature3">>, "local")),
                    ?assertNot(darklaunch:is_enabled(<<"feature3">>, "clownco")),
                    ?assertNot(darklaunch:is_enabled(<<"feature2">>, "local")),
                    ?assertMatch(ok, darklaunch:set_enabled(<<"feature2">>, "local", true)),
                    ?assert(darklaunch:is_enabled(<<"feature2">>, "local")),
                    ?assertMatch(ok, darklaunch:set_enabled(<<"feature3">>, "clownco", true)),
                    ?assert(darklaunch:is_enabled(<<"feature3">>, "clownco")),
                    ?assertMatch(ok, darklaunch:set_enabled(<<"feature2">>, "clownco", false)),
                    ?assertNot(darklaunch:is_enabled(<<"feature2">>, "clownco")),
                    InterimJson = <<"{\"feature2\": [\"local\"], \"feature3\": [\"local\", \"clownco\"]}">>,
                    ?_assertEqual(canonical_features(darklaunch:to_json()), canonical_features(InterimJson))
                end
            end
        }

     ]}.
