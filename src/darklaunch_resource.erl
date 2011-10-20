%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Noah Kantrowitz <noah@coderanger.net>
%% @author Christopher Maier <cm@opscode.com>
%% @copyright 2011 Opscode, Inc.

-module(darklaunch_resource).
-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         post_is_create/2,
         process_post/2,
         resource_exists/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include("darklaunch_resource.hrl").

%%------------------------------------------------------------------------------
%% Webmachine Resource Configuration Functions
%%------------------------------------------------------------------------------

init([]) -> {ok, #state{}}.

allowed_methods(Req, State) ->
    {['GET', 'POST'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

post_is_create(Req, State) ->
    {false, Req, State}.

process_post(ReqData, State=#state{action=Action,
                                   feature=Feature,
                                   org=Org}) ->
    JsonResponse = case {Action, Feature, Org} of
                       %% If POSTing to the server, replace the configuration with the body of the request.
                       %% Always returns {"ok": true}
                       {set_enabled, undefined, undefined} ->
                           ok = darklaunch:from_json(wrq:req_body(ReqData)),
                           <<"{\"ok\": true}">> ;
                       %% If feature, but no org, is defined, then set the feature status globally
                       {set_enabled, _, undefined} ->
                           IsEnabled = parse_set_enabled_body(ReqData),
                           ok = darklaunch:set_enabled(Feature, IsEnabled),
                           is_enabled_response(IsEnabled);
                       %% If feature and org are both defined, set the feature status for that org
                       {set_enabled, _, _} ->
                           IsEnabled = parse_set_enabled_body(ReqData),
                           ok = darklaunch:set_enabled(Feature, Org, IsEnabled),
                           is_enabled_response(IsEnabled)
                   end,

    ReqData1 = wrq:append_to_response_body(JsonResponse, ReqData),

    {true, ReqData1, State}.

resource_exists(Req, State) ->
    Action = case wrq:method(Req) of
        'GET' ->
            is_enabled;
        'POST' ->
            set_enabled
        end,
    Feature = case wrq:path_info(feature, Req) of
        undefined ->
            undefined;
        Any ->
            list_to_binary(Any)
        end,
    Org = wrq:path_info(org, Req),
    {true, Req, State#state{action=Action, feature=Feature, org=Org}}.

%%------------------------------------------------------------------------------
%% Application Functions
%%------------------------------------------------------------------------------

%% Generates the standard JSON response for individual feature tweaks (i.e. not
%% setting or retrieving the server configuration).
%%
%% {"ok": true,
%%  "enabled": true}
%%
is_enabled_response(Val) ->
    Response = {[{<<"ok">>, true},
                 {<<"enabled">>, Val}]},
    ejson:encode(Response).

%% If POSTing a tweak for an individual feature, a body must be present, and
%% of the form
%%
%% {"enabled": true}
%%
%% If no body is present, an exception is thrown with a message of 'no_request_body'.
%% If unexpected JSON is present, currently a "no matching case clause" exception
%% is thrown.
%%
%% TODO: Is this behavior (no matching clause) appropriate?
parse_set_enabled_body(Req) ->
    Bin = case wrq:req_body(Req) of
        undefined ->
            throw(no_request_body);
        Any ->
            Any
        end,
    case ejson:decode(Bin) of
        {[{<<"enabled">>, Val}]} when is_boolean(Val) ->
            Val
    end.

%%------------------------------------------------------------------------------
%% GET Requests: is the feature enabled?
%%------------------------------------------------------------------------------

%% If no feature or org is given (i.e., you just do a GET to the base server
%% endpoint), you get a JSON dump of the Darklaunch configuration
to_json(Req, #state{action=is_enabled, feature=undefined, org=undefined}=State) ->
    {darklaunch:to_json(), Req, State};

%% If no org is given, ask if feature is enabled globally
to_json(Req, #state{action=is_enabled, feature=Feature, org=undefined}=State) ->
    IsEnabled = darklaunch:is_enabled(Feature),
    {is_enabled_response(IsEnabled), Req, State};

%% If org and feature are given, ask if feature is enabled for that org
to_json(Req, #state{action=is_enabled, feature=Feature, org=Org}=State) ->
    IsEnabled = darklaunch:is_enabled(Feature, Org),
    {is_enabled_response(IsEnabled), Req, State}.
