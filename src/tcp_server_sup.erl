%%%-------------------------------------------------------------------
%% @doc
%%      tcp_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tcp_server_sup).

-behaviour(supervisor).

%% API
-export([start_child/1, start_socket/1, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
%%    SupFlags = {simple_one_for_one, 0, 1},
    SupFlags = #{
        strategy  => simple_one_for_one,
        intensity => 60,
        period    => 3600
    },
    %% We start our pool of empty listeners.
    %% We must do this in another, as it is a blocking process.
%%    Opts = [binary, {active, once}, {reuseaddr, true}], % {packet, raw},
%%    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
%%    io:format("Start listen socket ~p with Port ~p~n", [ListenSocket, Port]),
%%    spawn_link(fun empty_listeners/0),
    ChildSpecs = [
        #{
            id       => tcp_server,
            start    => {tcp_server, start_link, []},
            restart  => temporary,
            shutdown => 1000,
            type     => worker,
            modules  => [tcp_server]
        }
    ],
%%    ChildSpecs = {
%%        tcp_server,
%%        {tcp_server, start_link, [ListenSocket]},
%%        temporary,
%%        infinity,
%%        worker,
%%        [tcp_server]
%%    },
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_socket(Opts) ->
    supervisor:start_child(?MODULE, Opts).

%%empty_listeners() ->
%%    [start_socket() || _ <- lists:seq(1,20)],
%%    ok.

start_child(Server) ->
    supervisor:start_child(Server, []).
