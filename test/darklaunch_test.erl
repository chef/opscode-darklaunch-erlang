%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Noah Kantrowitz <noah@coderanger.net>
%% @copyright 2011 Opscode, Inc.

-module(darklaunch_test).

-include_lib("eunit/include/eunit.hrl").

scanenv([]) ->
    %% Fall back to /tmp in case none of
    %% the env vars are found.
    case filelib:is_dir("/tmp") of
        true ->
            "/tmp";
        false ->
            throw(not_found)
    end;
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
    {Keys} = jiffy:decode(Bin),
    SortedKeys = lists:sort([ canonical_org_features(K) || K <- Keys ]),
    iolist_to_binary(jiffy:encode({SortedKeys})).

%%------------------------------------------------------------------------------
%% Server Setup / Cleanup Functions
%%------------------------------------------------------------------------------

%% Sets up a testing darklaunch server.  Returns path to config file.
setup_darklaunch(ConfigJson, ReloadTime) ->
    {_, ConfigPath} = tempfile("darklaunch"),
    file:write_file(ConfigPath, ConfigJson),
    application:set_env(darklaunch, config, ConfigPath),
    application:set_env(darklaunch, reload_time, ReloadTime),
    darklaunch:start_link(),
    ConfigPath.

cleanup_darklaunch(ConfigPath) ->
    darklaunch:stop_link(),
    ok = file:delete(ConfigPath).

%%------------------------------------------------------------------------------

darklaunch_load_config_from_file_test_() ->
    {foreachx,
     fun(Json) ->
        {TempFile, TempFileName} = tempfile("darklaunch"),
        file:write(TempFile, Json),
        application:set_env(darklaunch, config, TempFileName),
        application:set_env(darklaunch, reload_time, 1000),
        {ok, _} = darklaunch:start_link(),
        {TempFile, TempFileName}
     end,
     fun(_Json, {_TempFile, TempFileName}) ->
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
                    ?_assert(darklaunch:is_enabled(<<"feature1">>, <<"clownco">>)),
                    ?_assertNot(darklaunch:is_enabled(<<"feature1">>, "someoneelse")),
                    ?_assertNot(darklaunch:is_enabled(<<"feature1">>, <<"someoneelse">>)),
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
                    ?_assert(darklaunch:is_enabled(<<"feature1">>, <<"clownco">>)),
                    ?_assert(darklaunch:is_enabled(<<"feature1">>, "local")),
                    ?_assertNot(darklaunch:is_enabled(<<"feature1">>, <<"someoneelse">>)),
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
        }
     ]}.

darklaunch_dupe_test() ->
    {TempFile, TempFileName} = tempfile("darklaunch"),
    file:write(TempFile, <<"{\"feature1\": true, \"feature1\": false}">>),
    application:set_env(darklaunch, config, TempFileName),
    application:set_env(darklaunch, reload_time, 1000),
    process_flag(trap_exit, true),
    ?assertMatch({error,{duplicate_key,<<"feature1">>}}, darklaunch:start_link()).

from_to_json_test_() ->
    {foreach,
     fun() ->
             {TempFile, TempFileName} = tempfile("darklaunch"),
             file:write(TempFile, <<"{}">>),
             os:cmd("cat " ++ TempFileName),
             application:set_env(darklaunch, config, TempFileName),
             application:set_env(darklaunch, reload_time, 1000),
             darklaunch:start_link(),
             error_logger:delete_report_handler(error_logger_tty_h),
             {TempFile, TempFileName}
     end,
     fun({_TempFile, TempFileName}) ->
             darklaunch:stop_link(),
             ok = file:delete(TempFileName)
     end,
     [
      {"empty config",
       fun() ->
               ?assertEqual(<<"{}">>, darklaunch:to_json())
       end},

      {"valid config",
       fun() ->
               Config = iolist_to_binary(["{\"feature1\": "
                                          "[\"clownco\", \"local\"],"
                                          "\"feature2\": true}"]),
               ok = darklaunch:from_json(Config),
               ?assertEqual(canonical_features(Config),
                            canonical_features(darklaunch:to_json()))
       end},

      {"bad JSON config",
       fun() ->
               Config = iolist_to_binary(["{\"feature1\": "
                                          "[\"clownco\", local],"
                                          "\"feature2\": true}"]),
               ?assertMatch({error, _}, darklaunch:from_json(Config)),
               %% old state is preserved
               ?assertEqual(<<"{}">>, darklaunch:to_json())
       end},

      {"Empty org lists are not round-tripped from to_json",
       %% semantically equal, but not literally identical
       fun() ->
               InConfig = iolist_to_binary(["{"
                                            "\"feature1\": true,"
                                            "\"feature2\": [\"clownco\"],"
                                            "\"feature3\": []"
                                            "}"]),
               OutConfig = iolist_to_binary(["{"
                                             "\"feature1\": true,"
                                             "\"feature2\": [\"clownco\"]"
                                             "}"]),
               ok = darklaunch:from_json(InConfig),
               ?assertEqual(canonical_features(OutConfig),
                            canonical_features(darklaunch:to_json()))
       end
      }
     ]}.


-spec rewrite_config_file_helper({invalid_json | valid_json,
                                  binary(),
                                  rewrite | from_json},
                                 {binary(), string()}) ->
                                        fun().
rewrite_config_file_helper({Valid, Json, Method},
                           {InitialJson, ConfigPath}) ->
    fun() ->
            %% Basic sanity checks
            ?assertNot(canonical_features(Json) =:=
                           canonical_features(InitialJson)),
            CurrentServerJson = darklaunch:to_json(),
            ?assertEqual(canonical_features(InitialJson),
                                    canonical_features(CurrentServerJson)),

            %% Change the configuration;
            %% Either overwrite the config file and allow it to get reloaded,
            %% or change it via `from_json`
            case Method of
                rewrite ->
                    %% Erlang's file modification time has a granularity of 1 second;
                    %% Darklaunch judges its config file to have changed based on
                    %% the modification time, so we need to sleep at least that long
                    %% before changing the file for it to get automatically
                    %% reloaded
                    test_server:sleep(2000),

                    %% write the new config to disk (and verify)
                    file:write_file(ConfigPath, Json),
                    ?assertMatch({ok, Json},
                                 file:read_file(ConfigPath)),

                    %% Wait a bit for it to automatically reload
                    test_server:sleep(1000);
                from_json ->
                    darklaunch:from_json(Json)
            end,

            case Valid of
                valid_json ->
                    ?assertEqual(canonical_features(Json),
                                 canonical_features(darklaunch:to_json()));
                invalid_json ->
                    ?assertEqual(canonical_features(CurrentServerJson),
                                 canonical_features(darklaunch:to_json()))
            end
    end.

reload_test_() ->
    {foreachx,
     %% SetupX
     fun(_X) ->
             InitialJson = <<"{\"new_theme\":true,\"sql_users\":true,\"sql_nodes\":[\"clownco\"]}">>,
             ConfigPath = setup_darklaunch(InitialJson, 500),
             {InitialJson, ConfigPath}
     end,
     %% CleanupX
     fun(_X, {_InitialJson, ConfigPath}) ->
             cleanup_darklaunch(ConfigPath)
     end,
     %% Pairs
     [
      {{invalid_json,
        <<"{\"quick_start\": \"barf\"}">>,
        rewrite},
       fun rewrite_config_file_helper/2},
      {{valid_json,
        <<"{\"flying_monkeys\":true}">>,
        rewrite},
       fun rewrite_config_file_helper/2},

      {{invalid_json,
        <<"{\"quick_start\": \"barf\"}">>,
        from_json},
       fun rewrite_config_file_helper/2},
      {{valid_json,
        <<"{\"flying_monkeys\":true}">>,
        from_json},
       fun rewrite_config_file_helper/2}

     ]
    }.

%%------------------------------------------------------------------------------
%% Test Feature Introspection / Modification Functions
%%------------------------------------------------------------------------------

enabled_test_helper({Feature, Org, Expected}, _) ->
    fun() ->
            case Org of
                nil ->
                    ?assertEqual(Expected, darklaunch:is_enabled(Feature)),
                    ?assertEqual(Expected, darklaunch:is_enabled(list_to_binary(Feature)));
                _ ->
                    ?assertEqual(Expected, darklaunch:is_enabled(Feature,
                                                                 Org)),
                    ?assertEqual(Expected, darklaunch:is_enabled(Feature,
                                                                 list_to_binary(Org))),
                    ?assertEqual(Expected, darklaunch:is_enabled(list_to_binary(Feature),
                                                                 Org)),
                    ?assertEqual(Expected, darklaunch:is_enabled(list_to_binary(Feature),
                                                                 list_to_binary(Org)))
            end
    end.

is_enabled_test_() ->
    {foreachx,
     %% SetupX
     fun(_X) ->
             Json = <<"{\"feature1\":true, \"feature2\": [\"clownco\", \"skynet\"]}">>,
             ConfigPath = setup_darklaunch(Json, 500),
             {Json, ConfigPath}
     end,
     %% CleanupX
     fun(_X, {_, ConfigPath}) ->
             cleanup_darklaunch(ConfigPath)
     end,
     %% Pairs
     [
      %% Global features
      {{"feature1", nil, true},
       fun enabled_test_helper/2},
      {{"unknown_feature", nil, false},
       fun enabled_test_helper/2},

      %% Org features
      {{"feature1", "skynet", true},
       fun enabled_test_helper/2},
      {{"unknown_feature", "skynet", false},
       fun enabled_test_helper/2},

      {{"feature2", "clownco", true},
       fun enabled_test_helper/2},
      {{"feature2", "skynet", true},
       fun enabled_test_helper/2},
      {{"feature2", "mungbeanpalace", false},
       fun enabled_test_helper/2}
     ]}.

set_enabled_test_helper({Feature, Org, OriginalStatus, DesiredStatus, EnabledOrgs, DisabledOrgs}, _) ->
    fun() ->
            {IsEnabledArgs, SetEnabledArgs} = case Org of
                                                  global -> {[Feature], [Feature, DesiredStatus]};
                                                  _ -> {[Feature, Org], [Feature, Org, DesiredStatus]}
                                              end,
            ?assertEqual(OriginalStatus, apply(darklaunch, is_enabled, IsEnabledArgs)),
            apply(darklaunch, set_enabled, SetEnabledArgs),
            ?assertEqual(DesiredStatus, apply(darklaunch, is_enabled, IsEnabledArgs)),

            [?assertEqual(true, darklaunch:is_enabled(Feature, O)) || O <- EnabledOrgs],
            [?assertEqual(false, darklaunch:is_enabled(Feature, O)) || O <- DisabledOrgs]
    end.

set_enabled_test_() ->
    {foreachx,
     %% SetupX
     fun(_X) ->
             Json = <<"{\"feature1\":true, \"feature2\": [\"clownco\", \"skynet\"]}">>,
             ConfigPath = setup_darklaunch(Json, 500),
             {Json, ConfigPath}
     end,
     %% CleanupX
     fun(_X, {_Json, ConfigPath}) ->
             cleanup_darklaunch(ConfigPath)
     end,
     %% Pairs
     [

      %% Global Feature Tests
      %% Enabling a global feature (that is already enabled)
      {{"feature1", global, true, true, ["clownco", "skynet", "opscode"],[]},
       fun set_enabled_test_helper/2},

      %% Enabling a global feature (that is NOT already enabled)
      {{"use_awesome_sauce", global, false, true, ["clownco", "skynet", "opscode"], []},
       fun set_enabled_test_helper/2},

      %% Disabling a global feature (that is already enabled)
      {{"feature1", global, true, false, [], ["clownco", "skynet", "opscode"]},
       fun set_enabled_test_helper/2},

      %% Disabling a global feature (that is already disabled)
      {{"use_awesome_sauce", global, false, false, [], ["clownco","skynet", "opscode"]},
       fun set_enabled_test_helper/2},


      %% Org Feature Tests
      %% Enabling a feature for an org (that is already enabled)

      {{"feature2", "clownco", true, true, ["clownco", "skynet"], []},
       fun set_enabled_test_helper/2},

      %% Enabling a feature for an org (that is NOT already enabled)
      {{"use_awesome_sauce", "clownco", false, true, ["clownco"], ["skynet"]},
       fun set_enabled_test_helper/2},

      %% Disabling a feature for an org (that is already enabled)
      %% Disabling a feature for one org should leave it enabled for another
      {{"feature2", "clownco", true, false, ["skynet"], ["clownco"]},
       fun set_enabled_test_helper/2},

      %% Disabling a feature for an org  (that is already disabled)
      {{"use_awesome_sauce", "clownco", false, false, [], ["clownco", "skynet"]},
       fun set_enabled_test_helper/2},

      %% Setting a global feature for an org should leave it a global
      {{"feature1", "clownco", true, true, ["clownco", "skynet", "randomorg"], []},
      fun set_enabled_test_helper/2},

      %% Enabling a feature for one org should not also enable it for another
      {{"use_awesome_sauce", "clownco", false, true, ["clownco"], ["skynet"]},
      fun set_enabled_test_helper/2}
     ]}.
