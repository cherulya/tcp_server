-module(server).

-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_socket/1]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {accept_sockets = [], users = #{}, auth_users = #{}, listen_socket}).

%%%===================================================================
%%% API
%%%===================================================================
start(Port) ->
    start_link(Port).

start_socket(N) ->
    gen_server:cast(?MODULE, {start_socket, N}).

stop(Pid) ->
    exit(Pid, normal).

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
    [{ok, _} = start_socket(N, ListenSocket) || N <- lists:seq(1, 5)],
    {ok, #state{users = AllUsers, listen_socket = ListenSocket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start_socket, N}, State = #state{listen_socket = ListenSocket}) ->
    start_socket(N, ListenSocket),
    {noreply, State};
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
    AuthSockets = maps:values(AuthUsers),
    case lists:member(FromSocket, AuthSockets) of
        true ->
            lists:foreach(
                fun
                    (Socket) when Socket =:= FromSocket ->
                        skip;
                    (Socket) ->
                        gen_tcp:send(Socket, Msg)
                end,
                AuthSockets
            );
        false ->
            gen_tcp:send(FromSocket, <<"you need to login before send message">>)
    end,
    {noreply, State};
handle_info({accept_socket, AcceptSocket}, State = #state{accept_sockets = AcceptSockets}) ->
    {noreply, State#state{accept_sockets = [AcceptSocket | AcceptSockets]}};
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.


start_socket(N, ListenSocket) ->
    tcp_server_sup:start_socket([self(), N, ListenSocket]).