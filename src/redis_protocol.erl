-module(redis_protocol).
%% exported functions
-export([start/1]).
%% cowboy callbacks
-export([start_link/4]).
%% private functions
-export([handle/4]).

%%-include_lib("eredis/include/eredis.hrl").

%% @doc Start the redis_protocol server.
start(Port) ->
    ok = application:start(cowboy),
    {ok, _} = cowboy:start_listener(
        ?MODULE, 10,
        cowboy_tcp_transport, [{port, Port}],
        ?MODULE, []),
    ok.

%% @private Spawn a process to handle a new connection.
start_link(ListenerPid, Socket, Transport, Options) ->
    Pid = spawn_link(?MODULE, handle, [ListenerPid, Socket, Transport, Options]),
    {ok, Pid}.

%% @private Handle a new connection.
handle(_ListenterPid, Socket, Transport, _Options) ->
    %% Wait for the acceptor to hand over the connection.
    %receive shoot -> ok after 500 -> io:format("exiting~n"), exit(nosocket) end,
    ok = Transport:setopts(Socket, [binary, {active, once}]), %%{packet, line}
    %ok = Transport:send(Socket, <<"Who are you?\n">>),
    Parser = eredis_parser:init(),

    read_line(Socket, Transport, Parser, <<>>),
    Transport:close(Socket).

read_line(Socket, Transport, Parser, Rest) ->
    Line = receive {tcp, Socket, ILine} -> ILine after 10000 -> exit(timeout) end,
    io:format("Line ~s~n", [binary_to_list(Line)]),
    P = parse(Socket, Transport, Parser, <<Rest/binary, Line/binary>>),
    io:format("Parse : ~p~n", [P]),
    case P of
        {ok, NewState} -> read_line(Socket, Transport, NewState, <<>>);
        {continue, NewState} -> read_line(Socket, Transport, NewState, Rest)
    end.

parse(Socket, Transport, State, Data) ->
    case eredis_parser:parse(State, Data) of
        {ok, Return, NewParserState} ->
            action(Socket, Transport, Return),
            {ok, NewParserState};
        {ok, Return, Rest, NewParserState} ->
            action(Socket, Transport, Return),
            parse(Socket, Transport, NewParserState, Rest);
        {continue, NewParserState} ->
            {continue, NewParserState};
        Error ->
            io:format("Error ~p~n", [Error]),
            {error, Error}
    end.

action(Socket, Transport, Action) ->
    io:format("Action ~p~n", [Action]),
    %% FIXME should write a response to the socket
    ok = Transport:send(Socket,<<"+OK\r\n">>),
    ok.
