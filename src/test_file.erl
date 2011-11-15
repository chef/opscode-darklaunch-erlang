%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @copyright 2011 Opscode, Inc.

%% Mocked version of the standard `file` module, used only for testing
%% the `darklaunch_app` module
-module(test_file).

-compile([export_all]).

-include_lib("kernel/include/file.hrl").

%% Mocked read_file_info
%% =============================================================================
read_file_info("/etc/opscode/dark_launch_features.json") ->
    {ok, #file_info{access=read_write}};
read_file_info("/etc/opscode/dorky_lunch_features.json") ->
    {ok, #file_info{access=read}};
read_file_info("/etc/opscode/file_not_present_writable.json") ->
    {error, enoent};
read_file_info("/etc/opscode/file_not_present_unwritable.json") ->
    {error, enoent};
read_file_info(_Path) ->
    {error, badarg}.

%% Mocked write_file
%% =============================================================================
write_file("/etc/opscode/dark_launch_features.json", _Bin) ->
    ok;
write_file("/etc/opscode/dorky_lunch_features.json", _Bin) ->
    {error, eaccess};
write_file("/etc/opscode/file_not_present_writable.json", _Bin) ->
    ok;
write_file("/etc/opscode/file_not_present_unwritable.json", _Bin) ->
    {error, eaccess};
write_file(_Path, _Bin) ->
    {error, badarg}.
