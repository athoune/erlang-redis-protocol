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

bulk_test() ->
    ?assertEqual(<<"$3\r\nfoo\r\n">>, encode(<<"foo">>)),
    ?assertEqual(<<"$3\r\nfoo\r\n">>, encode(foo)).

multi_bulk_test() ->
    ?assertEqual(<<"*2\r\n$5\r\nhello\r\n$5\r\nworld\r\n">>, encode([<<"hello">>, <<"world">>])),
    ?assertEqual(<<"*2\r\n$5\r\nhello\r\n$2\r\n42\r\n">>, encode(["hello", 42])).
