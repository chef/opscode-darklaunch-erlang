-module(darklaunch).

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
        {error, Reason, #state{}=ErrState} ->
            {reply, {error, Reason}, ErrState}
    end;
handle_call({from_json, Bin}, _From, #state{config_path = ConfigPath}=State) ->
    {Reply, State1} = case load_features(Bin, State) of
                          {error, Reason, ErrState} ->
                              {{error, Reason}, ErrState};
                          {ok, #state{}=NewState} ->
                              file:write_file(ConfigPath, Bin),
                              {ok, NewState}
                      end,
    {reply, Reply, State1};
handle_call(to_json, _From, #state{features = Features, org_features = OrgFeatures}=State) ->
    OrgFeatures1 = dict:fold(fun({Feature, Org}, true, Acc) ->
        dict:append(Feature, list_to_binary(Org), Acc)
    end, dict:new(), OrgFeatures),
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
            {ok, lists:foldl(fun(KV, Accum) -> parse_value(KV, Accum) end,
                             {[], []}, TopKeys)};
        Reason ->
            {error, Reason}
    end.

load_features(Bin, State) ->
    load_features(Bin, write_config, State).

load_features(Bin, FileInfo, State) ->
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
    end.

load_features(#state{config_path = ConfigPath} = State) ->
    case read_config(ConfigPath) of
        {ok, FileInfo, Bin} ->
            load_features(Bin, FileInfo, State);
        {error, Why} ->
            {error, Why}
    end.

check_features(#state{config_path=ConfigPath, mtime=MTime}=State) ->
    case file:read_file_info(ConfigPath) of
        {ok, #file_info{mtime=MTime}} ->
            {ok, State};
        {ok, #file_info{mtime=_MTime1} = FileInfo} ->
            {ok, Bin} = file:read_file(ConfigPath),
            load_features(Bin, FileInfo, State);
        Error ->
            {error, Error, State}
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

to_str(X) when is_list(X) ->
    X;
to_str(X) when is_binary(X) ->
    binary_to_list(X).
