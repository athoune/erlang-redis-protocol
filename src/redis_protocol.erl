-module(redis_protocol).
%% exported functions
-export([start/1]).
%% cowboy callbacks
-export([start_link/4]).
%% private functions
-export([handle/4]).

%%-include_lib("eredis/include/eredis.hrl").

-record(connection, {
    socket,
    transport,
    state = []
    }).

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
    Parser = eredis_parser:init(),
    read_line(#connection{socket= Socket, transport=Transport}, Parser, <<>>),
    Transport:close(Socket).

read_line(#connection{socket=Socket, transport=Transport} = Connection, Parser, Rest) ->
    ok = Transport:setopts(Socket, [binary, {active, once}]), %% TODO check options
    Line = receive {tcp, Socket, ILine} ->
        ILine
    after 30000 ->
        exit(timeout)
    end,
    case parse(Connection, Parser, <<Rest/binary, Line/binary>>) of
        {ok, ConnectionState, NewState} ->
            read_line(Connection#connection{state=ConnectionState}, NewState, <<>>);
        {continue, NewState} -> read_line(Connection, NewState, Rest);
        _ -> io:format("Oups le readline.")
    end.

parse(Connection, State, Data) ->
    case eredis_parser:parse(State, Data) of
        {ok, Return, NewParserState} ->
            {ok, ConnectionState} = action(Connection, Return),
            {ok, ConnectionState, NewParserState};
        {ok, Return, Rest, NewParserState} ->
            {ok, ConnectionState} = action(Connection, Return),
            parse(Connection#connection{state=ConnectionState}, NewParserState, Rest);
        {continue, NewParserState} ->
            {continue, NewParserState};
        Error ->
            io:format("Error ~p~n", [Error]),
            {error, Error}
    end.

action(#connection{socket = Socket, transport=Transport, state=State}, Action) ->
    io:format("Action ~p~n", [Action]),
    io:format("State ~p~n", [State]),
    ok = Transport:send(Socket,<<"+OK\r\n">>),
    {ok, [Action | State]}.
