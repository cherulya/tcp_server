%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. март 2025 18:06
%%%-------------------------------------------------------------------
-module(main_sup).
-author("user").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{
        strategy   => one_for_all,
        intensity  => 5,
        period     => 1
    },
    ChildSpecs = [
        #{
            id       => tcp_server_sup,
            start    => {tcp_server_sup, start_link, []},
            restart  => permanent,
            shutdown => 5000,
            type     => supervisor
        }
%%        #{
%%            id       => tcp_controller,
%%            start    => {tcp_controller, start_link, []},
%%            restart  => transient,
%%            shutdown => 5000,
%%            type     => worker
%%        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
