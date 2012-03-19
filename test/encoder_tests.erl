-module(encoder_tests).

-include_lib("eunit/include/eunit.hrl").

encode(Var) ->
    list_to_binary(redis_protocol_encoder:encode(Var)).

ok_test() ->
    ?assertEqual(<<"+OK\r\n">>, encode(ok)).

int_test() ->
    ?assertEqual(<<":42\r\n">>, encode(42)).

error_test() ->
    ?assertEqual(<<"-Argh\r\n">>, encode({error, <<"Argh">>})).
