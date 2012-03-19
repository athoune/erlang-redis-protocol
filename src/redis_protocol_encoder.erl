-module(redis_protocol_encoder).
-author('mathieu@garambrogne.net').

%% Larg parts of this code came from eredis.erl

-export([encode/1]).

-include_lib("eredis/include/eredis.hrl").

encode(ok) ->
    encode({single_line, <<"OK">>});
encode({single_line, Arg}) ->
    [<<$+>>, Arg, <<?NL>>];
encode({error, Arg}) ->
    [<<$->>, Arg, <<?NL>>];
encode(Arg) when is_integer(Arg) ->
    [<<$:>>, integer_to_list(Arg), <<?NL>>];
encode(Arg) when is_binary(Arg) ->
    [<<$$>>, integer_to_list(iolist_size(Arg)), <<?NL>>, Arg, <<?NL>>];
encode(Args) when is_list(Args) ->
    ArgCount = [<<$*>>, integer_to_list(length(Args)), <<?NL>>],
    ArgsBin = lists:map(fun encode/1, lists:map(fun to_binary/1, Args)),

    [ArgCount, ArgsBin];
encode(Arg) ->
    encode(to_binary(Arg)).

to_binary(X) when is_list(X)    -> list_to_binary(X);
to_binary(X) when is_atom(X)    -> list_to_binary(atom_to_list(X));
to_binary(X) when is_binary(X)  -> X;
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_float(X)   -> throw({cannot_store_floats, X});
to_binary(X)                    -> term_to_binary(X).
