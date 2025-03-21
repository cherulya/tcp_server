-module(client).

-include("tcp_server.hrl").

-behaviour(gen_server).

%% API
-export([connect/1, connect/2, create_account/2, login/2, send/1, stop/0]).
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(client_state, {login, password, socket, host, port, users}).

%%%===================================================================
%%% API
%%%===================================================================
connect(Port) ->
    connect("localhost", Port).

connect(Host, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port], []).

create_account(Login, Password) ->
    gen_server:cast(?MODULE, {create_account, Login, Password}).

login(Login, Password) ->
    gen_server:cast(?MODULE, {login, Login, Password}).

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
    case gen_tcp:connect(Host, Port, [binary, {packet, 4}]) of
        {ok, Socket} ->
            io:format("Client ~p connects to ~p:~p with socket ~p~n", [self(), Host, Port, Socket]),
            {ok, #client_state{socket = Socket, host = Host, port = Port}};
        {error, Reason} ->
            io:format("Client ~p can't connect to ~p:~p by reason ~p~n", [self(), Host, Port, Reason]),
            {stop, normal}
    end.

handle_call(_Request, _From, State = #client_state{}) ->
    {reply, ok, State}.

handle_cast({login, Login, Password}, State = #client_state{socket = Socket}) ->
    Msg = <<"login:"/utf8, Login/binary, "/password:"/utf8, Password/binary>>,
    gen_tcp:send(Socket, Msg),
    {noreply, State};
handle_cast({create_account, Login, Password}, State = #client_state{socket = Socket}) ->
    Msg = <<"new_login:"/utf8, Login/binary, "/new_password:"/utf8, Password/binary>>,
    gen_tcp:send(Socket, Msg),
    {noreply, State};
handle_cast({send, Msg}, State = #client_state{socket = Socket}) ->
    Result = gen_tcp:send(Socket, Msg),
    io:format("Client ~p sends to Socket ~p msg ~p with result: ~p~n", [self(), Socket, Msg, Result]),
    {noreply, State};
handle_cast(stop, State = #client_state{socket = Socket}) ->
    io:format("Client ~p closes connection and stops~n", [self()]),
    gen_tcp:close(Socket),
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Msg}, State = #client_state{socket = _Socket}) when
   Msg =:= <<"auth_error">> orelse
   Msg =:= <<"login already created">> orelse
   Msg =:= <<"can't create account">> orelse
   Msg =:= <<"already_connection">> ->
    io:format("Client ~p got error message: ~p~n", [self(), Msg]),
    {noreply, State};
handle_info({tcp, _Socket, Msg}, State) ->
    io:format("Client ~p got message: ~p~n", [self(), Msg]),
    {noreply, State};
handle_info({tcp_closed, Socket}, State) ->
    io:format("~p: tcp closed normal with socket: ~p~n", [self(), Socket]),
    {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, State) ->
    io:format("~p: error ~p tcp closed with socket: ~p~n", [self(), Reason, Socket]),
    {stop, {tcp_error, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #client_state{socket = Socket}) ->
    io:format("Close socket ~p~n", [Socket]),
    gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.