%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% @copyright 2011 Opscode, Inc.

-module(darklaunch).

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

-define(SERVER, ?MODULE).

%%-------------------------------------------------------------------
%% Data Type: state
%% where:
%%   config_path: fully-qualified path to JSON config file
%%   mtime: last modification time of the config file
%%   features: global features; key is feature, value is a
%%     boolean indicating its activation status
%%   org_features: organization-specific features; key is a
%%     {Feature, Org} tuple, value is boolean indicating if
%%     the feature is enabled for that organization
%%-------------------------------------------------------------------
-record(state, {config_path :: string(),
                mtime :: file:date_time(),
                features :: dict(),
                org_features :: dict()}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         is_enabled/2,
         is_enabled/1,
         set_enabled/3,
         set_enabled/2,
         reload_features/0,
         from_json/1,
         to_json/0,
         stop_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-type bin_or_string() :: binary() | string().

-spec is_enabled(bin_or_string(), bin_or_string())
                -> boolean().
is_enabled(Feature, Org) ->
    gen_server:call(?SERVER, {enabled, ensure_bin(Feature), ensure_bin(Org)}).

-spec is_enabled(bin_or_string())
                -> boolean().
is_enabled(Feature) ->
    gen_server:call(?SERVER, {enabled, ensure_bin(Feature)}).

set_enabled(Feature, Org, Val) when is_binary(Feature),
                              is_boolean(Val) ->
    gen_server:call(?SERVER, {set_enabled, Feature, ensure_bin(Org), Val}).

set_enabled(Feature, Val) when is_binary(Feature),
                          is_boolean(Val) ->
    gen_server:call(?SERVER, {set_enabled, Feature, Val}).

reload_features() ->
    gen_server:call(?SERVER, reload_features).

from_json(Bin) when is_binary(Bin) ->
    gen_server:call(?SERVER, {from_json, Bin}).

to_json() ->
    gen_server:call(?SERVER, to_json).

stop_link() ->
    gen_server:call(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    %% Assume these are going to be present in the config and crash
    %% if they aren't
    {ok, ConfigPath} = application:get_env(darklaunch, config),
    {ok, ReloadTime} = application:get_env(darklaunch, reload_time),
    timer:send_interval(ReloadTime, reload_features),
    case load_features(#state{config_path=ConfigPath}) of
        {ok, InitState} ->
            {ok, InitState};
        {error, Reason, _} ->
            {stop, Reason}
    end.

handle_call({enabled, Feature}, _From, #state{features = Features}=State) ->
    Ans = case dict:find(Feature, Features) of
              error ->
                  false;
              {ok, Val} ->
                  Val
          end,
    {reply, Ans, State};
handle_call({enabled, Feature, Org}, _From, #state{features = Features, org_features = OrgFeatures}=State) ->
    Ans = case dict:find({Feature, Org}, OrgFeatures) of
              error ->
                  case dict:find(Feature, Features) of
                      error ->
                          false;
                      {ok, Val1} ->
                          Val1
                  end;
              {ok, Val} ->
                  Val
          end,
    {reply, Ans, State};
handle_call({set_enabled, _Feature, _Val}, _From, State) ->
    {reply, ok, State};
handle_call({set_enabled, _Feature, _Org, _Val}, _From, State) ->
    {reply, ok, State};
handle_call(reload_features, _From, State) ->
    case check_features(State) of
        {ok, #state{}=NewState} ->
            {reply, ok, NewState};
        %% If the new config is bad for some reason, keep the server going
        %% with the original (i.e., good) state
        {error, Reason, #state{}=OriginalState} ->
            {reply, {error, Reason}, OriginalState}
    end;

%%------------------------------------------------------------------------------
%% from_json
%%
%% Given a JSON string, parse and load it as a configuration file, and overwrite
%% the contents of the original configuration file.
%%------------------------------------------------------------------------------
handle_call({from_json, Bin}, _From, #state{config_path = ConfigPath}=State) ->
    {Reply, State1} = case load_features(Bin, State) of
                          {error, Reason, ErrState} ->
                              {{error, Reason}, ErrState};
                          {ok, #state{}=NewState} ->
                              file:write_file(ConfigPath, Bin),
                              {ok, NewState}
                      end,
    {reply, Reply, State1};

%%------------------------------------------------------------------------------
%% to_json
%%
%% Regenerate a JSON configuration string from the current state of the server.
%% Contains both global and organization-specific feature configurations.
%%------------------------------------------------------------------------------
handle_call(to_json, _From, #state{features = Features, org_features = OrgFeatures}=State) ->
    OrgFeatures1 = dict:fold(fun({Feature, Org}, true, Acc) ->
        dict:append(Feature, Org, Acc)
    end, dict:new(), OrgFeatures),
    %% Because we ensure no keys are duplicated going in, we just arbitrarily choose the
    %% global feature in the merge function
    Features1 = dict:merge(fun(_Key, _Value1, Value2) -> Value2 end, OrgFeatures1, Features),
    {reply, ejson:encode({dict:to_list(Features1)}), State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reload_features, State) ->
    case check_features(State) of
        {ok, #state{}=NewState} ->
            {noreply, NewState};
        {error, Reason, #state{}=ErrState} ->
            error_logger:error_report({error, Reason, erlang:get_stacktrace()}),
            {noreply, ErrState}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Function: write_config/2
%% Purpose: Write a JSON configuration to the config file and
%%          generate a new server state from it.
%% Args: Bin, State
%% Returns: New state record, identical to State, but with new mtime.
%%          This is only called from load_features/3, which handles syncing
%%          the details of the new config with the server's state
%%------------------------------------------------------------------------------
write_config(Bin, #state{config_path = ConfigPath} = State) ->
    ok = file:write_file(ConfigPath, Bin),
    {ok, FileInfo} = file:read_file_info(ConfigPath),
    State#state{mtime=FileInfo#file_info.mtime}.

parse_config(Bin) ->
    {TopKeys} = try
                    ejson:decode(Bin)
                catch
                    throw:Why ->
                        {{bad_json, Why}}
                end,
    case validate_config_json(TopKeys) of
        ok ->
            {ok, lists:foldl(fun parse_value/2,
                             {[], []},
                             TopKeys)};
        Reason ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% Function: load_features/2
%% Purpose: Load configuration information from a JSON string that does not
%%          come from a file.  Used in the `from_json` handler to load arbitrary
%%          JSON and overwrite the config file.
%%------------------------------------------------------------------------------
load_features(Bin, State) ->
    load_features(Bin, write_config, State).

%%------------------------------------------------------------------------------
%% Function: load_features/3
%% Purpose: Load configuration from Bin.  If FileInfo = write_config, the contents
%%          of Bin will be written to the config file.
%% Returns: a new state record, reflective of the contents of Bin
%%------------------------------------------------------------------------------
-spec load_features(Bin::binary(), FileInfo::#file_info{} | write_config, State::#state{})
                   -> {ok, #state{}} | {error, Reason::term(), #state{}}.
load_features(Bin, FileInfo, State) ->
    try
        case parse_config(Bin) of
            {error, Reason} ->
                {error, Reason, State};
            {ok, {Features, OrgFeatures}} ->
                State1 = case FileInfo of
                             write_config ->
                                 write_config(Bin, State);
                             I ->
                                 State#state{mtime=I#file_info.mtime}
                         end,
                {ok, State1#state{features=dict:from_list(Features),
                                  org_features=dict:from_list(OrgFeatures)}}
        end
    catch
        %% A key in the configuration doesn't have the proper kind of value;
        %% Note this fact, and continue using the original state
        throw:{invalid_config, Why} ->
            error_logger:error_report({error, Why, erlang:get_stacktrace()}),
            {error, invalid_config, State}
    end.

%%------------------------------------------------------------------------------
%% Function: load_features/1
%% Purpose: Perform the initial loading of state for the server.  Reads the
%%          config_path information from State to locate and parse the JSON
%%          configuration file.
%% Returns: Returns {error, Why} if the configuration file could not be read
%%          {error, Reason, State} if the configuration could not be parsed,
%%          or {ok, State} if parsing was successful
%%------------------------------------------------------------------------------
load_features(#state{config_path = ConfigPath} = State) ->
    case read_config(ConfigPath) of
        {ok, FileInfo, Bin} ->
            load_features(Bin, FileInfo, State);
        {error, Why} ->
            {error, Why}
    end.

%%------------------------------------------------------------------------------
%% Function: check_features/1
%% Purpose: Generate new server state if the configuration on disk has changed.
%%------------------------------------------------------------------------------
%% -spec check_features(#state{})
%%                     -> {ok, UpToDateState::#state{}} | {error, term(), OriginalState::#state{}}.
check_features(#state{config_path=ConfigPath, mtime=MTime}=State) ->
    case file:read_file_info(ConfigPath) of
        %% Modification time is unchanged; State need not change
        {ok, #file_info{mtime=MTime}} ->
            {ok, State};
        %% Config file has changed; reload
        {ok, #file_info{mtime=_MTime1} = FileInfo} ->
            {ok, Bin} = file:read_file(ConfigPath),
            load_features(Bin, FileInfo, State);
        Error ->
            {error, Error, State}
    end.

parse_value({Key, Val}, {GlobalConfig, OrgConfig}) when is_boolean(Val) ->
    {[{Key, Val}|GlobalConfig], OrgConfig};
parse_value({Key, Orgs}, {GlobalConfig, OrgConfig}) when is_list(Orgs) ->
   {GlobalConfig, [{{Key, Org}, true} || Org <- Orgs] ++ OrgConfig};
parse_value({Key, Val}, {_GlobalConfig, _OrgConfig}) ->
    throw ({invalid_config,
            {value_not_boolean_or_list, {Key, Val}}}).

%%------------------------------------------------------------------------------
%% Function: read_config
%% Purpose: Retrieve binary file contents and file information about the JSON
%%          configuration file
%%------------------------------------------------------------------------------
-spec read_config(string())
                 -> {ok, #file_info{}, binary()} | {error, Reason::term()}.
read_config(ConfigPath) ->
    case file:read_file(ConfigPath) of
        {ok, Bin} ->
            case file:read_file_info(ConfigPath) of
                {ok, FileInfo} ->
                    {ok, FileInfo, Bin};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

validate_config_json({bad_json, Reason}) ->
    {bad_json, Reason};
validate_config_json([]) ->
    ok;
validate_config_json([{Key, _}|T]) ->
    case lists:keymember(Key, 1, T) of
        false ->
            validate_config_json(T);
        true ->
            {duplicate_key, Key}
    end.

ensure_bin(L) when is_list(L) ->
    list_to_binary(L);
ensure_bin(B) when is_binary(B) ->
    B.
