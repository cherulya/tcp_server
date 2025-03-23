-define(ACCEPT_COUNT, 5).

-record(tcp_user, {
    password :: term() | undefined,
    login :: binary() | undefined
}).