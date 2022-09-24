-module(test_server).
-export([start/0, server/1]).

start() ->
    case gen_tcp:listen(8080, [binary, {active, false}, {packet, 0}]) of
        {ok, ListenSock} ->
            start_servers(1, ListenSock),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error, Reason} ->
            {error, Reason}
    end.

start_servers(0, _) ->
    ok;
start_servers(Num, LS) ->
    spawn(?MODULE, server, [LS]),
    start_servers(Num - 1, LS).

server(ListenSock) ->
    io:format("~s", ["server listening..."]),
    case gen_tcp:accept(ListenSock) of
        {ok, Socket} ->
            loop(Socket),
            server(ListenSock);
        Other ->
            io:format("accept returned ~w - goodbye!~n", [Other]),
            ok
    end.

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    io:format("~s~n", ["server processing..."]),
    receive
        {tcp, Socket, Data} ->
            io:format("~s~s~n", ["server----", binary_to_list(Data)]),
            io:format("~s~w~n", ["socket: ", Socket]),
            % Not implemented in this example
            Answer = "AAAAAA" ++ binary_to_list(Data),
            gen_tcp:send(Socket, Answer),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Socket ~w closed [~w]~n", [Socket, self()]),
            ok
    end.
