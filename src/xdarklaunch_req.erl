%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%%-------------------------------------------------------------------
%% @author Mark Anderson <mark@opscode.com>
%% @copyright (C) 2013, Opscode, Inc.
%% @doc
%%
%% @end
%% Created : 20 Mar 2013 by Mark Anderson <>
%%-------------------------------------------------------------------
-module(xdarklaunch_req).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
         parse_header/1,
         is_enabled/2,
         is_enabled/3
        ]).

-ifdef(TEST).
-export([parse_header_int/1]).
-endif.

-define(XDARKLAUNCH_HEADER, "X-Ops-Darklaunch").

-record(xdarklaunch, {
          orgname :: string(),
          values = dict:new() :: dict()
         }).

-type header_fun() :: fun( (string()) -> binary() | undefined).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Fetch and parse the darklaunch header
%%
%% GetHeader is a fun taking the header name and returing a binary;
%% e.g. chef_wm_util:get_header_fun
%%
%% Expects a header in the format
%% KEY1=VALUE1;KEY2=VALUE2...  KEY is expected to be alphanumeric plus _ VALUE can be any
%% non-whitespace string, but currently we only expect 0,1
-spec parse_header(header_fun()) -> #xdarklaunch{}.
parse_header(GetHeader) ->
    %% This header is inserted by nginx, so we shouldn't need to be too paranoid about its contents.
    Header = GetHeader(?XDARKLAUNCH_HEADER),
    parse_header_int(Header).

parse_header_int(undefined) ->
    no_header;
parse_header_int(<<>>) ->
    no_header;
parse_header_int(Header) ->
    Dl = #xdarklaunch{orgname=undefined},
    %% It's worth noting that the re library takes strings or binaries, and we force binary output.
    HeaderParts = re:split(Header, <<";">>, [{return, binary}]),
    %% This is chosen to be relatively hardened against garbage; , whitespaces, nonstandard chars
    {ok, Re} = re:compile(<<"\\s*([[:alnum:]\_]+)\\s*=\\s*(\\w+)\\s*">>),
    Extract = fun(Part, Dict) -> parse_part(Part, Dict, Re) end,
    Values = lists:foldl(Extract, Dl#xdarklaunch.values, HeaderParts),
    Dl#xdarklaunch{values = Values}.


parse_part(Part, Dict, Re) ->
    case re:run(Part, Re, [{capture, all, binary}]) of
        {match, [_, Key, Value]} ->
            dict:store(Key, parse_value(Value), Dict);
        _ -> Dict
    end.


parse_value(<<"0">>) -> 0;
parse_value(<<"1">>) -> 1;
parse_value(<<"false">>) -> 0;
parse_value(<<"true">>) -> 1;
parse_value(X) -> X.

%% @doc Fetch the darklaunch value if available
%% If a key is missing from the darklaunch headers, we throw.
%%
-spec is_enabled(binary(), #xdarklaunch{} | no_header) -> boolean().
is_enabled(Key, no_header) ->
    %% Fall back to old dark launch
    darklaunch:is_enabled(Key);
is_enabled(Key, #xdarklaunch{values=Dict}) ->
    case dict:find(Key, Dict) of
        {ok, V} -> is_enabled_helper(V);
        error -> throw({darklaunch_missing_key, Key})
    end.

%% @doc Fetch the darklaunch value if available, otherwise return a default value
%% An alternate API with a default value for those users that have a sensible option
%% available
%%
-spec is_enabled(binary(), #xdarklaunch{} | no_header, boolean()) -> boolean().
is_enabled(Key, no_header, _Default) ->
    %% Fall back to old dark launch
    darklaunch:is_enabled(Key);
is_enabled(Key, #xdarklaunch{values=Dict}, Default) ->
    case dict:find(Key, Dict) of
        {ok, V} -> is_enabled_helper(V);
        error -> Default
    end.

is_enabled_helper(0) -> false;
is_enabled_helper(1) -> true;
is_enabled_helper(true) -> true;
is_enabled_helper(_) ->  false.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
