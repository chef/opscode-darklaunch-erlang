-module(dark_launch_test).

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

dark_launch_test_() ->
    {foreachx,
     fun(Json) ->
        {TempFile, TempFileName} = tempfile("dark_launch"),
        file:write(TempFile, Json),
        application:set_env(chef_common, dark_launch_config, TempFileName),
        application:set_env(chef_common, dark_launch_reload_time, 1000),
        dark_launch:start_link(),
        {TempFile, TempFileName}
     end,
     fun(_Json, {_TempFile, TempFileName}) ->
        meck:unload(),
        dark_launch:stop_link(),
        ok = file:delete(TempFileName)
     end,
     [
        {
            <<"{\"feature1\": true}">>,
            fun(_, _) ->
                [
                    ?_assert(dark_launch:is_enabled(<<"feature1">>)),
                    ?_assert(dark_launch:is_enabled(<<"feature1">>, "clownco")),
                    ?_assertNot(dark_launch:is_enabled(<<"feature2">>)),
                    ?_assertNot(dark_launch:is_enabled(<<"feature2">>, "clownco"))
                ]
            end
        },
        {
            <<"{\"feature1\": [\"clownco\"]}">>,
            fun(_, _) ->
                [
                    ?_assertNot(dark_launch:is_enabled(<<"feature1">>)),
                    ?_assert(dark_launch:is_enabled(<<"feature1">>, "clownco")),
                    ?_assertNot(dark_launch:is_enabled(<<"feature1">>, "someoneelse"))
                ]
            end
        },
        {
            <<"{\"feature1\": [\"clownco\", \"local\"], \"feature2\": true}">>,
            fun(_, _) ->
                [
                    ?_assertNot(dark_launch:is_enabled(<<"feature1">>)),
                    ?_assert(dark_launch:is_enabled(<<"feature1">>, "clownco")),
                    ?_assert(dark_launch:is_enabled(<<"feature1">>, "local")),
                    ?_assertNot(dark_launch:is_enabled(<<"feature1">>, "someoneelse")),
                    ?_assert(dark_launch:is_enabled(<<"feature2">>)),
                    ?_assert(dark_launch:is_enabled(<<"feature2">>, "clownco")),
                    ?_assert(dark_launch:is_enabled(<<"feature2">>, "local")),
                    ?_assert(dark_launch:is_enabled(<<"feature2">>, "someoneelse"))
                ]
            end
        },
        {
            <<"{\"feature1\": true}">>,
            fun(_, {TempFile, _TempFileName}) ->
                fun() ->
                    ?assert(dark_launch:is_enabled(<<"feature1">>)),
                    ?assertNot(dark_launch:is_enabled(<<"feature2">>)),
                    test_server:sleep(1000), %% Sleep for one second to make sure the timestamp differs
                    file:position(TempFile, 0),
                    file:write(TempFile, <<"{\"feature2\": true}">>),
                    ok = dark_launch:reload_features(),
                    ?assertNot(dark_launch:is_enabled(<<"feature1">>)),
                    ?assert(dark_launch:is_enabled(<<"feature2">>))
                end
            end
        }
     ]}.
