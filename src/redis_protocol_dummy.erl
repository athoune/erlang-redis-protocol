-module(redis_protocol_dummy).
-behaviour(redis_protocol).

-export([start/0]).

-export([handle/4]).

start() ->
    redis_protocol:start(6379, ?MODULE).

handle(Socket, Transport, State, Action) ->
    io:format("Action ~p~n", [Action]),
    io:format("State ~p~n", [State]),
    ok = Transport:send(Socket,<<"+OK\r\n">>),
    {ok, [Action | State]}.
