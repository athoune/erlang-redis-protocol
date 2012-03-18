Redis protocol
==============

Let the others talking to your erlang server with the redis protocol.

You can build a fake redis server or a slave.

Just like the node version : [node redis protocol](https://github.com/athoune/node-redis-protocol)

Try it
------

    $ make
    $ ./start.sh
    1> redis_protocol_dummy:start().

In an other terminal :

    $ redis-cli
    redis 127.0.0.1:6379> hello world

Use it
------

Hereis a dummy example

```erlang
-module(redis_protocol_dummy).
-behaviour(redis_protocol).

-export([start/0]).

-export([handle/4]).

start() ->
    redis_protocol:start(6379, ?MODULE). %% It's the minimalistic start, with no option

%% Each messages land here
%%  * You need Transport and Socket to answer.
%%  * State is used for every messages in this conection, you can modify it
%% Action is a binary list with redis like command
handle(Connection, State, Action) ->
    io:format("Action ~p~n", [Action]),
    io:format("State ~p~n", [State]),
    ok = redis_protocol:answer(Connection, ok),
    {ok, [Action | State]}.
```

TODO
----

 * _ Code cleanup, it's a library not an apllication
 * _ Some unit test
 * _ Behavior with simple example

Licence
-------

MIT © Mathieu Lecarme, 2012
