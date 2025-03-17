%%%-------------------------------------------------------------------
%%% @doc
%%%     tcp_server public API
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    R = tcp_server_sup:start_link(),
    io:format("tcp_server_app ~p~n", [R]),
    R.

stop(_State) ->
    ok.