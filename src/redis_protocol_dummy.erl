-module(redis_protocol_dummy).
-behaviour(redis_protocol).

-export([start/0]).

-export([handle/3]).

start() ->
    redis_protocol:start(6379, ?MODULE).

handle(Connection, State, Action) ->
    io:format("Action ~p~n", [Action]),
    io:format("State ~p~n", [State]),
    ok = redis_protocol:answer(Connection, ok),
    {ok, [Action | State]}.
