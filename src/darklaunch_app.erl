%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright 2011 Opscode, Inc.

-module(darklaunch_app).

-behaviour(application).

-include_lib("kernel/include/file.hrl").

%% In order to unit-test things in this class, we need to mock some functions
%% from the `file` module.  However, this is a "sticky" module, and mocking
%% it globally will screw the whole system up.
%%
%% As an alternative, we'll simply define a macro that replaces the standard
%% `file` module with a mock testing module, but only when we're compiling
%% and running testing code.
%%
%% As a result, any references to `file` will be replaced with `?file`.
%%
-ifdef(TEST).
-define(file, test_file).
-compile([export_all]).
-else.
-define(file, file).
-endif.

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % If the config file is present and writable, or non-existent but writable, go ahead and
    % start up the supervisor.  Otherwise bail out!
    {ok, ConfigPath} = application:get_env(darklaunch,config),
    case ensure_accessible_config(ConfigPath) of
        ok ->
            darklaunch_sup:start_link();
        Error ->
            error_logger:error_report([invalid_configuration,
                                      {configPath, ConfigPath},
                                      Error]),
            Error
    end.

stop(_State) ->
    ok.

ensure_accessible_config(Path) ->
    case ?file:read_file_info(Path) of
        {ok, #file_info{access=read_write}} ->
            ok;
        {ok, _} ->
            {error, insufficient_privileges_for_config_file};
        {error, enoent} ->
            %% If the file doesn't exist, go ahead and write an empty JSON hash
            %% to the path.  This way, the application won't choke when it
            %% tries to initialize from non-existent, non-JSON data
            case ?file:write_file(Path, <<"{}">>) of
                ok ->
                    error_logger:warning_report([empty_configuration_written,
                                                 {configPath, Path},
                                                 "No file existed at the given path, so an empty configuration was written"]),
                    ok;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.
