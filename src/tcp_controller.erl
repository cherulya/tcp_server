%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. март 2025 18:11
%%%-------------------------------------------------------------------
-module(tcp_controller).
-author("user").

-behaviour(gen_server).

%% API
-export([start/1, stop/1]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {accept_sockets = []}).

%%%===================================================================
%%% API
%%%===================================================================
start(Port) ->
    start_link(Port).

stop(Pid) ->
    exit(Pid, normal).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port]) ->
    Opts = [binary, {active, once}, {reuseaddr, true}], % {packet, raw},
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    io:format("Start listen socket ~p with Port ~p~n", [ListenSocket, Port]),
    _ListUsers = [],
    R = [{ok, _} = tcp_server_sup:start_socket([self(), N, ListenSocket]) || N <- lists:seq(1, 5)],
    {ok, #state{}}.

handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info({send_msg, FromSocket, Msg}, State = #state{accept_sockets = AcceptSockets}) ->
    lists:foeach(
        fun
            (AS) when AS =:= FromSocket ->
                skip;
            (AS) ->
                gen_tcp:send(AS, io_lib:format(Msg, []))
        end,
        AcceptSockets
    ),
    {noreply, State};
handle_info({accept_socket, AcceptSocket}, State = #state{accept_sockets = AcceptSockets}) ->
    {noreply, State#state{accept_sockets = [AcceptSocket | AcceptSockets]}};
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
