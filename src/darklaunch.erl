-module(darklaunch).

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

-define(SERVER, ?MODULE).
-define(FEATURES_TABLE, darklaunch_features).
-define(ORGS_TABLE, darklaunch_orgs).
-define(ETS_OPTS, [set, public, named_table, {write_concurrency, true},
                   {read_concurrency, true}]).

-record(state, {config_path,
                reload_interval,
                mtime}).

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

is_enabled(Feature, Org) when is_binary(Feature),
                              is_list(Org) ->
    gen_server:call(?SERVER, {enabled, Feature, Org}).

is_enabled(Feature) when is_binary(Feature) ->
    gen_server:call(?SERVER, {enabled, Feature}).

set_enabled(Feature, Org, Val) when is_binary(Feature),
                              is_list(Org),
                              is_boolean(Val) ->
    gen_server:call(?SERVER, {set_enabled, Feature, Org, Val}).

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
    timer:send_after(ReloadTime, reload_features),
    ets:new(?FEATURES_TABLE, ?ETS_OPTS),
    ets:new(?ORGS_TABLE, ?ETS_OPTS),
    {ok, load_features(#state{config_path=ConfigPath, reload_interval=ReloadTime})}.

handle_call({enabled, Feature}, _From, State) ->
    Ans = ets:member(?FEATURES_TABLE, Feature),
    {reply, Ans, State};
handle_call({enabled, Feature, Org}, _From, State) ->
    Ans = case ets:member(?ORGS_TABLE, {Feature, Org}) of
              false ->
                  ets:member(?FEATURES_TABLE, Feature);
              true ->
                  true
          end,
    {reply, Ans, State};
handle_call({set_enabled, Feature, true}, _From, State) ->
    ets:insert(?FEATURES_TABLE, {Feature}),
    {reply, ok, State};
handle_call({set_enabled, Feature, false}, _From, State) ->
    ets:delete_object(?FEATURES_TABLE, {Feature}),
    {reply, ok, State};
handle_call({set_enabled, Feature, Org, true}, _From, State) ->
    ets:insert(?ORGS_TABLE, {{Feature, Org}}),
    {reply, ok, State};
handle_call({set_enabled, Feature, Org, false}, _From, State) ->
    ets:delete_object(?ORGS_TABLE, {{Feature, Org}}),
    {reply, ok, State};
handle_call(reload_features, _From, State) ->
    {reply, ok, check_features(State)};
handle_call({from_json, Bin}, _From, #state{config_path = ConfigPath}=State) ->
    file:write_file(ConfigPath, Bin),
    {reply, ok, load_features(State)};
handle_call(to_json, _From, State) ->
    %% Fix (or pin) tables to get consistent view while we iterate
    %% Tables are fixed together to limit the window of inconsistency
    %% between the two
    ets:safe_fixtable(?FEATURES_TABLE, true),
    ets:safe_fixtable(?ORGS_TABLE, true),
    Features = features_to_list(),
    Orgs = orgs_to_list(),
    ets:safe_fixtable(?FEATURES_TABLE, false),
    ets:safe_fixtable(?ORGS_TABLE, false),
    {reply, ejson:encode({Features ++ Orgs}), State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reload_features, #state{reload_interval=ReloadInterval}=State) ->
    State1 = check_features(State),
    timer:send_after(ReloadInterval, reload_features),
    {noreply, State1};
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
            {Keys} = ejson:decode(Bin),
            %% Clean out all data and load from scratch
            ets:delete_all_objects(?FEATURES_TABLE),
            ets:delete_all_objects(?ORGS_TABLE),
            [load_tables(Key) || Key <- Keys],
            State#state{mtime=FileInfo#file_info.mtime};
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

load_tables({Key, true}) ->
    ets:insert(?FEATURES_TABLE, {Key});
load_tables({_Key, false}) ->
    ok;
load_tables({Key, Orgs}) when is_list(Orgs) ->
    [ets:insert(?ORGS_TABLE, {{Key, to_str(Org)}}) || Org <- Orgs].

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

features_to_list() ->
    ets:foldr(fun({Feature}, Accum) -> [{Feature, true}|Accum] end,
              [], ?FEATURES_TABLE).

orgs_to_list() ->
    F = fun({{Feature, Org0}}, Accum) ->
                Org = list_to_binary(Org0),
                case dict:find(Feature, Accum) of
                    error ->
                        dict:store(Feature, [Org], Accum);
                    {ok, Orgs} ->
                        dict:store(Feature, [Org|Orgs], Accum)
                end end,
    OrgFeatures = ets:foldr(F, dict:new(), ?ORGS_TABLE),
    dict:to_list(OrgFeatures).
