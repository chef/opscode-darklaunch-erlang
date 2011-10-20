%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Noah Kantrowitz <noah@coderanger.net>
%% @copyright 2011 Opscode, Inc.

%% Request handler state
-record(state, {
    action,
    feature,
    org
}).
