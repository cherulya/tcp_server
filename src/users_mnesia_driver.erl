%%%-------------------------------------------------------------------
%%% @doc
%%%     CR (Create | Read) users
%%% @end
%%%-------------------------------------------------------------------
-module(users_mnesia_driver).

-include("tcp_server.hrl").

-define(TRANSACTION(F), mnesia:transaction(fun() -> F end)).
-define(TABLE_NAME, tcp_user).

-export([
    storage_init/0,
    create_user/2,
    get_user/1,
    get_all_users/0
]).

%%====================================================================
%% API
%%====================================================================
-spec storage_init() -> ok | {error, Reason :: term()}.
storage_init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    TableAttributes = [
        {disc_copies, [node()]},
        {type, ordered_set},
        {attributes, record_info(fields, tcp_user)}
        ],
    case mnesia:create_table(?TABLE_NAME, TableAttributes) of
        {atomic, ok} ->
            mnesia:wait_for_tables([?TABLE_NAME], 30000);
        {aborted, {already_exists, _Name}} ->
            mnesia:wait_for_tables([?TABLE_NAME], 30000);
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec create_user(Login :: binary(), Password :: term()) -> ok | {error, Reason :: term()}.
create_user(Login, Password) ->
    UserData = #tcp_user{login = Login, password = Password},
    case ?TRANSACTION(mnesia:write(?TABLE_NAME, UserData, write)) of
        {atomic, ok} ->
            io:format("safe user: ~p~n", [Login]),
            ok;
        {aborted, Reason} ->
            io:format("can't safe user: ~p", [Login]),
            {error, Reason}
    end.

-spec get_user(Login :: binary()) -> {ok, #tcp_user{}} | {error, Reason :: term()}.
get_user(Login) ->
    case mnesia:dirty_read(?TABLE_NAME, Login) of
        [#tcp_user{login = Login, password = Password}] ->
            {ok, #{Login => Password}};
        Reason ->
            io:format("can't get user by reason: ~p~n", [Reason]),
            error
    end.

-spec get_all_users() -> map().
get_all_users() ->
    Users = mnesia:dirty_select(?TABLE_NAME, [{'$1',[],['$1']}]),
    lists:foldl(
        fun(UserSpec = #tcp_user{login = Login}, Acc) ->
            Acc#{Login => UserSpec#tcp_user.password}
        end,
        #{},
        Users
    ).
