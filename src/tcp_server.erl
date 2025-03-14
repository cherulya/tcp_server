-module(tcp_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Socket :: term()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Socket) ->
    %% Start accepting requests
    %% We must cast this to the worker's process, as it blocks it.
    gen_server:cast(self(), accept),
    {ok, #state{socket = Socket}}.

handle_cast(accept, State = #state{socket = ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    %% Boot a new listener to replace this one.
    tcp_server_sup:start_socket(),
    send(AcceptSocket, "Hello", []),
    {noreply, State#state{socket = AcceptSocket}};
handle_cast(_, State) ->
    {noreply, State}.


handle_info({tcp, Socket, "quit"++_}, State) ->
    gen_tcp:close(Socket),
    {stop, normal, State};
handle_info({tcp, Socket, Msg}, State) ->
    send(Socket, Msg, []),
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

%% Send a message back to the client
send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str ++ "~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.
