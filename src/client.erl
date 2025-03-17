%%%-------------------------------------------------------------------
%%% @author ulya
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2025 6:25 PM
%%%-------------------------------------------------------------------
-module(client).
-author("ulya").

-behaviour(gen_server).

%% API
-export([start/1, start/2, send/1, stop/0]).
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(client_state, {socket, host, port}).

%%%===================================================================
%%% API
%%%===================================================================
start(Port) ->
    start("localhost", Port).

start(Host, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port], []).

send(Msg) ->
    gen_server:cast(?MODULE, {send, Msg}).

stop() ->
    gen_server:cast(?MODULE, stop).

start_link(Host, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Port]) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, raw}]),
    io:format("Client ~p connects to ~p:~p with socket ~p~n", [self(), Host, Port, Socket]),
    {ok, #client_state{socket = Socket, host = Host, port = Port}}.

handle_call(_Request, _From, State = #client_state{}) ->
    {reply, ok, State}.

handle_cast({send, Msg}, State = #client_state{socket = Socket}) ->
    Size = byte_size(Msg),
    Header = <<Size:16/integer>>,
    Result = gen_tcp:send(Socket, <<Header/binary, Msg/binary>>),
    io:format("Client ~p send to Socket ~p msg ~p with result: ~p~n", [self(), Socket, Msg, Result]),
    {noreply, State};
handle_cast(stop, State = #client_state{socket = Socket}) ->
    io:format("Client ~p closes connection and stops~n", [self()]),
    gen_tcp:close(Socket),
    {noreply, State};
handle_cast(_Request, State = #client_state{}) ->
    {noreply, State}.

handle_info({tcp, _Socket, Msg}, State = #client_state{}) ->
    io:format("Client ~p got message: ~p~n", [self(), Msg]),
    {noreply, State};
handle_info(stop, State = #client_state{socket = Socket}) ->
    io:format("Client ~p closes connection and stops~n", [self()]),
    gen_tcp:close(Socket),
    {noreply, State};
handle_info(_Info, State = #client_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #client_state{}) ->
    ok.

code_change(_OldVsn, State = #client_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
