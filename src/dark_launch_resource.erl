%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(dark_launch_resource).
-export([init/1, 
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

%% Request handler state
-record(state, {
    action,
    feature,
    org
}).

init([]) -> {ok, #state{}}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

resource_exists(Req, State) ->
    Action = case wrq:method(Req) of
        'GET' ->
            is_enabled
        end,
    Feature = case wrq:path_info(feature, Req) of
        undefined ->
            undefined;
        Any ->
            list_to_binary(Any)
        end,
    Org = wrq:path_info(org, Req),
    {true, Req, State#state{action=Action, feature=Feature, org=Org}}.

is_enabled_response(Val) ->
    Response = {[{<<"is_enabled">>, Val}]},
    ejson:encode(Response).

to_json(Req, #state{action=is_enabled, feature=Feature, org=undefined}=State) ->
    IsEnabled = dark_launch:is_enabled(Feature),
    {is_enabled_response(IsEnabled), Req, State};
to_json(Req, #state{action=is_enabled, feature=Feature, org=Org}=State) ->
    IsEnabled = dark_launch:is_enabled(Feature, Org),
    {is_enabled_response(IsEnabled), Req, State}.
