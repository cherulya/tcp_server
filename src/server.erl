-module(server).

-behaviour(gen_server).

%% API
-export([start/1, stop/1]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {accept_sockets = [], users = #{}}).

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
    Opts = [binary, {active, once}, {reuseaddr, true}],
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    io:format("Start listen socket ~p with Port ~p~n", [ListenSocket, Port]),
    MnesiaResult = users_mnesia_driver:storage_init(),
    AllUsers = users_mnesia_driver:get_all_users(),
    io:format("mnesia ~p~n", [MnesiaResult]),
    [{ok, _} = tcp_server_sup:start_socket([self(), N, ListenSocket]) || N <- lists:seq(1, 5)],
    {ok, #state{users = AllUsers}}.

handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info({login, FromSocket, Login, Password}, State = #state{users = Users}) ->
    Answer =
        case maps:find(Login, Users) of
            {ok, StoragePassword} when Password =:= StoragePassword ->
                <<"ok">>;
            {ok, _CorrectPassword} ->
                <<"wrong_password">>;
            error ->
                <<"user_not_found">>
        end,
    gen_tcp:send(FromSocket, io_lib:format(Answer, [])),
    {noreply, State};
handle_info({create_account, FromSocket, Login, Password}, State = #state{users = Users}) ->
    {Answer, NewState} =
        case maps:find(Login, Users) of
            {ok, _Password} ->
                {<<"already_created">>, State};
            error ->
                case users_mnesia_driver:create_user(Login, Password) of
                    ok ->
                        {<<"ok">>, State#state{users = Users#{Login => Password}}};
                    error ->
                        {<<"can_not_create">>, State}
                end
        end,
    gen_tcp:send(FromSocket, io_lib:format(Answer, [])),
    {noreply, NewState};
handle_info({send_msg, FromSocket, Msg}, State = #state{accept_sockets = AcceptSockets}) ->
    lists:foreach(
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