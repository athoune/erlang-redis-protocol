-module(redis_protocol).
%% exported functions
-export([start/2, start/3]).
%% cowboy callbacks
-export([start_link/4]).
%% private functions
-export([handle_new_connection/5]).
%% behavior
-export([behaviour_info/1]).

%% helper
-export([answer/2]).

-record(connection, {
    socket,
    transport,
    state,
    options = [],
    module
    }).

%% @doc Start the redis_protocol server.
start(Port, Mod) ->
    start(Port, Mod, []).

%% Options
%%  timeout, default 30000 (30 seconds) connection timeout
%%  state, [], default state
start(Port, Mod, Options) ->
    ok = application:start(cowboy),
    {ok, _} = cowboy:start_listener(
        ?MODULE, 10,
        cowboy_tcp_transport, [{port, Port}],
        ?MODULE, {Mod, Options}),
    ok.

behaviour_info(callbacks) -> [{handle, 3}];
behaviour_info(_) -> undefined.

%% @private Spawn a process to handle a new connection.
start_link(ListenerPid, Socket, Transport, {Mod, Options}) ->
    Pid = spawn_link(?MODULE, handle_new_connection, [ListenerPid, Socket, Transport, Mod, Options]),
    {ok, Pid}.

%% @private Handle a new connection.
handle_new_connection(_ListenterPid, Socket, Transport, Mod, Options) ->
    Parser = eredis_parser:init(),
    State = proplists:get_value(state, Options, []), %% FIXME handle when state is a function
    read_line(#connection{
        socket = Socket,
        transport = Transport,
        options = Options,
        state = State,
        module = Mod}, Parser, <<>>),
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

parse(#connection{socket = Socket, transport=Transport, state=HandleState, module=Mod} = Connection, State, Data) ->
    case eredis_parser:parse(State, Data) of
        {ok, Return, NewParserState} ->
            {ok, ConnectionState} = Mod:handle({Socket, Transport}, HandleState, Return),
            {ok, ConnectionState, NewParserState};
        {ok, Return, Rest, NewParserState} ->
            {ok, ConnectionState} = Mod:handle({Socket, Transport}, HandleState, Return),
            parse(Connection#connection{state=ConnectionState}, NewParserState, Rest);
        {continue, NewParserState} ->
            {continue, NewParserState};
        Error ->
            io:format("Error ~p~n", [Error]),
            {error, Error}
    end.

answer({Socket, Transport}, Answer) ->
    Transport:send(Socket, redis_protocol_encoder:encode(Answer)).
