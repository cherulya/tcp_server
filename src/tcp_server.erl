-module(tcp_server).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {n, parent, listen_socket, accept_socket}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Parent, N, ListenSocket) ->
    gen_server:start_link(?MODULE, [Parent, N, ListenSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Parent, N, ListenSocket]) ->
    io:format("Start tcp server ~p~n", [N]),
    self() ! accept,
    {ok, #state{n = N, parent = Parent, listen_socket = ListenSocket}}.


handle_cast(_, State) ->
    {noreply, State}.

handle_info(accept, State = #state{n = N, parent = Parent, listen_socket = ListenSocket}) ->
    io:format("Socket #~p, wait for client ~n", [N]),
    case gen_tcp:accept(ListenSocket) of
        {ok, AcceptSocket}  ->
            io:format("session #~p, started Accept socket ~p~n", [N, AcceptSocket]),
            Parent ! {accept_socket, AcceptSocket},
            {noreply, State#state{accept_socket = AcceptSocket}};
        {error, Reason} ->
            io:format("Can't Accept by reason ~p~n", [Reason]),
            {stop, Reason, State}
    end;
handle_info({tcp, Socket, Msg}, State = #state{parent = Parent}) ->
    io:format("tcp server Socket ~p, Msg ~p~n", [Socket, Msg]),
    case binary:split(Msg, [<<":">>, <<"/">>], [global]) of
        [<<"login">>, Login, <<"password">>, Password] ->
            Parent ! {login, Socket, Login, Password};
        [<<"new_login">>, Login, <<"new_password">>, Password] ->
            Parent ! {create_account, Socket, Login, Password};
        [Msg] ->
            Parent ! {send_msg, Socket, Msg}
    end,
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, Socket}, State = #state{n = N, listen_socket = ListenSocket}) ->
    io:fwrite("tcp_closed: ~p~n", [Socket]),
    tcp_server_sup:start_socket([self(), N, ListenSocket]),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    {stop, normal, State};
handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

handle_call(_E, _From, State) ->
    {noreply, State}.

terminate(_Reason, _Tab) ->
    ok.

code_change(_OldVersion, Tab, _Extra) ->
    {ok, Tab}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
