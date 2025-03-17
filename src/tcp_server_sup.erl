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
    SupFlags = #{
        strategy  => simple_one_for_one,
        intensity => 60,
        period    => 3600
    },
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
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_socket(Opts) ->
    supervisor:start_child(?MODULE, Opts).

start_child(Server) ->
    supervisor:start_child(Server, []).
