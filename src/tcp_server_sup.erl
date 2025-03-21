%%%-------------------------------------------------------------------
%% @doc
%%      tcp_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tcp_server_sup).

-behaviour(supervisor).

%% API
-export([start_child/1, delete_child/1, terminate_child/1, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_child(ChildId :: pid()) -> ok | {error, Error :: atom()}.
start_child(Opts) ->
    supervisor:start_child(?MODULE, Opts).

-spec delete_child(ChildId :: pid()) -> ok | {error, Error :: atom()}.
delete_child(ChildId) ->
    supervisor:delete_child(?MODULE, ChildId).

-spec terminate_child(ChildId :: pid()) -> ok | {error, Error :: atom()}.
terminate_child(ChildId) ->
    supervisor:terminate_child(?MODULE, ChildId).

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