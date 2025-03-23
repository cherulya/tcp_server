-module(tcp_server).

-behaviour(gen_server).

-include("tcp_server.hrl").

%% API
-export([
    start/1,
    stop/0,
    start_socket/1,
    delete_socket/1
]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    accept_sockets = [],
    users = #{},
    auth_users = #{},
    listen_socket,
    workers
}).

%%%===================================================================
%%% API
%%%===================================================================
start(Port) ->
    start_link(Port).

start_socket(N) ->
    gen_server:cast(?MODULE, {start_socket, N}).

delete_socket(Socket) ->
    gen_server:cast(?MODULE, {delete_socket, Socket}).

stop() ->
    gen_server:cast(?MODULE, stop).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port]) ->
    Opts = [binary, {active, true}, {reuseaddr, true}, {packet, 4}],
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    io:format("Server starts listen socket ~p with port ~p~n", [ListenSocket, Port]),
    users_mnesia_driver:storage_init(),
    AllUsers = users_mnesia_driver:get_all_users(),
    TcpServerWorkers =
        lists:foldl(
            fun(N, Acc) ->
                {ok, Pid} = start_socket(N, ListenSocket),
                [Pid | Acc]
            end,
            [],
            lists:seq(1, ?ACCEPT_COUNT)
        ),
    {ok, #state{users = AllUsers, listen_socket = ListenSocket, workers = TcpServerWorkers}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start_socket, N}, State = #state{listen_socket = ListenSocket, workers = Workers}) ->
    {ok, Pid} = start_socket(N, ListenSocket),
    {noreply, State#state{workers = [Pid | Workers]}};
handle_cast({delete_socket, Socket}, State = #state{accept_sockets = AcceptSocket, auth_users = AuthUsers}) ->
    io:format("delete socket ~p~n", [Socket]),
    UpdateAcceptSocket =  lists:delete(Socket, AcceptSocket),
    UpdateAuthUsers = maps:filter(fun(_Login, StorageSocket) -> StorageSocket =/= Socket end, AuthUsers),
    {noreply, State#state{accept_sockets = UpdateAcceptSocket, auth_users = UpdateAuthUsers}};
handle_cast(stop, State = #state{accept_sockets = AcceptSockets, workers = Workers}) ->
    stop(AcceptSockets, Workers),
    {stop, normal, State};
handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info({login, FromSocket, Login, Password}, State = #state{users = Users, auth_users = AuthUsers}) ->
    {Answer, NewState} =
        case {maps:find(Login, Users), maps:find(Login, AuthUsers)} of
            {{ok, CorrectPassword}, error} when Password =:= CorrectPassword ->
                UpdateAuthUsers = AuthUsers#{Login => FromSocket},
                {<<"login success">>, State#state{auth_users = UpdateAuthUsers}};
            {_, {ok, _Socket}} ->
                {<<"already_connection">>, State};
            _ ->
                {<<"auth_error">>, State}
        end,
    gen_tcp:send(FromSocket, Answer),
    {noreply, NewState};
handle_info({create_account, FromSocket, Login, Password}, State = #state{users = Users, auth_users = AuthUsers}) ->
    {Answer, NewState} =
        case maps:find(Login, Users) of
            {ok, _Password} ->
                {<<"login already created">>, State};
            error ->
                case users_mnesia_driver:create_user(Login, Password) of
                    ok ->
                        UpdateAuthUsers = AuthUsers#{Login => FromSocket},
                        NS = State#state{users = Users#{Login => Password}, auth_users = UpdateAuthUsers},
                        {<<"create account success">>, NS};
                    error ->
                        {<<"can't create account">>, State}
                end
        end,
    gen_tcp:send(FromSocket, Answer),
    {noreply, NewState};
handle_info({send_msg, FromSocket, Msg}, State = #state{auth_users = AuthUsers}) ->
    {FromLogin, ToSockets} =
        maps:fold(
            fun
                (Login, Socket, {_LoginAcc, ToSocketsAcc}) when Socket =:= FromSocket ->
                    {Login, ToSocketsAcc};
                (_Login, Socket, {LoginAcc, ToSocketsAcc}) ->
                    {LoginAcc, [Socket | ToSocketsAcc]}
            end,
            {undefined, []},
            AuthUsers
        ),
    case FromLogin of
        undefined ->
            gen_tcp:send(FromSocket, <<"you need to login before send message">>);
        _ ->
            FullMsg = <<"From: "/utf8, FromLogin/binary, ", Message: "/utf8, Msg/binary>>,
            [gen_tcp:send(Socket, FullMsg) || Socket <- ToSockets]
    end,
    {noreply, State};
handle_info({accept_socket, AcceptSocket}, State = #state{accept_sockets = AcceptSockets}) ->
    {noreply, State#state{accept_sockets = [AcceptSocket | AcceptSockets]}};
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{accept_sockets = AcceptSockets, workers = Workers}) ->
    stop(AcceptSockets, Workers),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_socket(N, ListenSocket) ->
    tcp_server_sup:start_child([self(), N, ListenSocket]).

stop(AcceptSockets, Workers) ->
    lists:foreach(
        fun(AcceptSocket) ->
            io:format("Close socket ~p~n", [AcceptSocket]),
            gen_tcp:close(AcceptSocket)
        end,
        AcceptSockets
    ),
    lists:foreach(
        fun(Worker) ->
            tcp_server_sup:terminate_child(Worker),
            tcp_server_sup:delete_child(Worker)
        end,
        Workers
    ).