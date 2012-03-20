-module(protocol_test).

-include_lib("eunit/include/eunit.hrl").

start() ->
    redis_protocol_dummy:start(),
    {ok, C} = eredis:start_link(),
    C.

stop(Client) ->
    eredis:stop(Client),
    application:stop(cowboy),
    ok.

protocol_test_() ->
    {foreach,
    fun start/0,
    fun stop/1,
    [fun ask_for_ok/1]}.

ask_for_ok(Client) ->
    A = eredis:q(Client, ["Hello", "World"]),
    io:format("Response : ~p~n", [A]),
    [?_assertEqual({ok, <<"OK">>}, A)].
