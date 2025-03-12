%%%-------------------------------------------------------------------
%% @doc
%%      tcp_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tcp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
    {ok, ListenSocket} = gen_tcp:listen(5000, [{active, once}]),
    %% We start our pool of empty listeners.
    %% We must do this in another, as it is a blocking process.
%%    spawn_link(fun empty_listeners/0),
    ChildSpecs = [
        #{
            id       => tcp_server,
            start    => {tcp_server, start_link, [ListenSocket]},
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
start_socket() ->
    supervisor:start_child(?MODULE, []).


empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.