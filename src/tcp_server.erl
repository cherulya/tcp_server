-module(tcp_server).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {n, parent, listen_socket, accept_socket}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(N, ListenSocket) ->
    gen_server:start_link(?MODULE, [self(), N, ListenSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Parent, N, ListenSocket]) ->
    io:format("Start tcp server ~p~n", [N]),
    %% Start accepting requests
    %% We must cast this to the worker's process, as it blocks it.
    self() ! accept,
    {ok, #state{n = N, parent = Parent, listen_socket = ListenSocket}}.


handle_cast(_, State) ->
    {noreply, State}.

handle_info(accept, State = #state{n = N, parent = Parent, listen_socket = ListenSocket}) ->
    io:format("Socket #~p, wait for client ~n", [N]),
    case gen_tcp:accept(ListenSocket) of
        {ok, AcceptSocket}  ->
            io:format("session #~p, started Accept socket ~p~n", [N, AcceptSocket]),
            %% Boot a new listener to replace this one.
%%            R  = tcp_server_sup:start_socket([N, ListenSocket]),
%%%%            self() ! accept,
%%            inet:setopts(AcceptSocket, [{active,once}]),
%%            R = supervisor:start_child(Parent, []),
%%            io:format("start child ~p Parent ~p~n", [R, Parent]),
            {noreply, State#state{accept_socket = AcceptSocket}};
        {error, Reason} ->
            io:format("Can't Accept by reason ~p~n", [Reason]),
            {stop, Reason, State}
    end;
handle_info({tcp, Socket, Msg}, State) ->
    io:format("tcp server Socket ~p, Msg ~p~n", [Socket, Msg]),
    ok = gen_tcp:send(Socket, io_lib:format(Msg, [])),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
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
