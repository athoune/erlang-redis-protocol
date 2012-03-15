Redis protocol
==============

Let the others talking to your erlang server with the redis protocol.

You can build a fake redis server or a slave.

Just like the node version : [node redis protocol](https://github.com/athoune/node-redis-protocol)

Try it
------

    $ make
    $ ./start.sh
    1> redis_protocol:start(6379).

In an other terminal :

    $ redis-cli
    redis 127.0.0.1:6379> hello world

TODO
----

 * _ Code cleanup, it's a library not an apllication
 * _ Some unit test
 * _ Behavior with simple example

Licence
-------

MIT © Mathieu Lecarme, 2012
