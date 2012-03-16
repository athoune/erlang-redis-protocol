-module(redis_protocol).
%% exported functions
-export([start/1, start/2]).
%% cowboy callbacks
-export([start_link/4]).
%% private functions
-export([handle/4]).

%%-include_lib("eredis/include/eredis.hrl").

-record(connection, {
    socket,
    transport,
    state,
    options = []
    }).

%% @doc Start the redis_protocol server.
start(Port) ->
    start(Port, []).

%% Options
%%  timeout, default 30000 (30 seconds) connection timeout
%%  state, [], default state
start(Port, Options) ->
    ok = application:start(cowboy),
    {ok, _} = cowboy:start_listener(
        ?MODULE, 10,
        cowboy_tcp_transport, [{port, Port}],
        ?MODULE, Options),
    ok.

%% @private Spawn a process to handle a new connection.
start_link(ListenerPid, Socket, Transport, Options) ->
    Pid = spawn_link(?MODULE, handle, [ListenerPid, Socket, Transport, Options]),
    {ok, Pid}.

%% @private Handle a new connection.
handle(_ListenterPid, Socket, Transport, Options) ->
    Parser = eredis_parser:init(),
    State = proplists:get_value(state, Options, []), %% FIXME handle when state is a function
    read_line(#connection{
        socket = Socket,
        transport = Transport,
        options = Options,
        state = State}, Parser, <<>>),
    Transport:close(Socket).

read_line(#connection{socket=Socket, transport=Transport, options=Options} = Connection, Parser, Rest) ->
    ok = Transport:setopts(Socket, [binary, {active, once}]),
    Timeout = proplists:get_value(timeout, Options, 30000),
    Line = receive {tcp, Socket, ILine} ->
        ILine
    after Timeout ->
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
