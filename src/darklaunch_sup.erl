%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright 2011 Opscode, Inc.

-module(darklaunch_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = [?CHILD(darklaunch, worker)] ++ configure_rest_interface(),
    {ok, { {one_for_one, 10, 10}, Children} }.

%% Internal functions
configure_rest_interface() ->
    case read_rest_addr_port() of
        undefined ->
            error_logger:info_msg("Configuring darklaunch for embedded mode~n"),
            [];
        {Addr, Port} ->
            {ok, Dispatch} = file:consult(filename:join([filename:dirname(code:which(?MODULE)),
                                                         "..", "priv", "dispatch.conf"])),
            WebConfig = [{ip, Addr},
                         {port, Port},
                         {log_dir, "priv/log"},
                         {dispatch, Dispatch}],
            [{webmachine_mochiweb,
              {webmachine_mochiweb, start, [WebConfig]},
              permanent, 5000, worker, dynamic}]
    end.

read_rest_addr_port() ->
    case { envy:get(darklaunch, listen_ip, undefined, string),
           envy:get(darklaunch, listen_port, undefined, non_neg_integer) } of
        {Addr, Port} when Addr == undefined orelse Port == undefined ->
            undefined;
        {Addr, Port} ->
           error_logger:info_msg("Configuring darklaunch for standalone mode (~s:~p)~n",
                                   [Addr, Port]),
           {Addr, Port}
     end.
