-module(dark_launch).

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

-define(SERVER, ?MODULE).

-record(state, {config_path,
                mtime,
                features,
                org_features}).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         is_enabled/2,
         is_enabled/1,
         reload_features/0,
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

is_enabled(Feature, Org) when is_binary(Feature),
                              is_list(Org) ->
    gen_server:call(?SERVER, {enabled, Feature, Org}).

is_enabled(Feature) when is_binary(Feature) ->
    gen_server:call(?SERVER, {enabled, Feature}).

reload_features() ->
    gen_server:call(?SERVER, reload_features).

stop_link() ->
    gen_server:call(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    %% Assume these are going to be present in the config and crash
    %% if they aren't
    {ok, ConfigPath} = application:get_env(chef_common, dark_launch_config),
    {ok, ReloadTime} = application:get_env(chef_common, dark_launch_reload_time),
    timer:send_interval(ReloadTime, reload_features),
    {ok, load_features(#state{config_path=ConfigPath})}.

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
handle_call(reload_features, _From, State) ->
    {reply, ok, check_features(State)};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reload_features, State) ->
    {noreply, check_features(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
load_features(State = #state{config_path = ConfigPath}) ->
    case read_config(ConfigPath) of
        {ok, FileInfo, Bin} ->
            {TopKeys} = ejson:decode(Bin),
            {Features, OrgFeatures} = lists:foldl(fun(KV, Accum) -> parse_value(KV, Accum) end,
                                                  {[], []}, TopKeys),
            State#state{mtime=FileInfo#file_info.mtime,
                        features=dict:from_list(Features),
                        org_features=dict:from_list(OrgFeatures)};
        Error ->
            %% Crash process if we can't read the file
            throw(Error)
    end.

check_features(#state{config_path=ConfigPath, mtime=MTime}=State) ->
    case file:read_file_info(ConfigPath) of
        {ok, #file_info{mtime=MTime}} ->
            State;
        {ok, #file_info{mtime=MTime1}} ->
            load_features(State#state{mtime=MTime1});
        Error ->
            %% Crash process if we can't read the file
            throw(Error)
    end.

parse_value({Key, Val}, {GlobalConfig, OrgConfig}) when is_boolean(Val) ->
    {[{Key, Val}|GlobalConfig], OrgConfig};
parse_value({Key, Orgs}, {GlobalConfig, OrgConfig}) when is_list(Orgs) ->
    {GlobalConfig, [{{Key, to_str(Org)}, true} || Org <- Orgs] ++ OrgConfig}.

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

to_str(X) when is_list(X) ->
    X;
to_str(X) when is_binary(X) ->
    binary_to_list(X).
