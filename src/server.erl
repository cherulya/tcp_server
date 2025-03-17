-module(server).

%% API
-export([
    start/1,
    stop/1]).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------
start(Port) ->
    Opts = [binary, {active, once}, {reuseaddr, true}], % {packet, raw},
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    io:format("Start listen socket ~p with Port ~p~n", [ListenSocket, Port]),
    _ListUsers = [],
    R = [{ok, _} = tcp_server_sup:start_socket([N, ListenSocket]) || N <- lists:seq(1, 5)],
    {ok, R}.

%% @doc Stop gen_tcp_server.
-spec stop(pid()) -> true.
stop(Pid) ->
    exit(Pid, normal).