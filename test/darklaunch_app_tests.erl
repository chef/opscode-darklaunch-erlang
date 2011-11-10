%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @copyright 2011 Opscode, Inc.

-module(darklaunch_app_tests).


-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

ensure_accessible_config_test() ->
    ?assertEqual(ok,
                 darklaunch_app:ensure_accessible_config("/etc/opscode/dark_launch_features.json")),
    ?assertEqual({error, insufficient_privileges_for_config_file},
                 darklaunch_app:ensure_accessible_config("/etc/opscode/dorky_lunch_features.json")),
    ?assertEqual(ok,
                 darklaunch_app:ensure_accessible_config("/etc/opscode/file_not_present_writable.json")),
    ?assertEqual({error, eaccess},
                 darklaunch_app:ensure_accessible_config("/etc/opscode/file_not_present_unwritable.json")),
    ?assertEqual({error, badarg},
                 darklaunch_app:ensure_accessible_config("/blah.json")).
