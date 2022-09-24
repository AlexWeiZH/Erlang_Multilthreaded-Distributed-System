-module(client).
-export([client/0]).
-import(io, [fwrite/1]).

client() ->
    % get the name and ip from input
    {ok, [Name, IP]} = io:fread("Name and IP -> ", "~s ~s"),
    io:format("~s ~s~n", [Name, IP]),
    % connect to server
    {ok, Sock} = gen_tcp:connect("localhost", 8080, [
        binary,
        {active, false},
        {packet, 0}
    ]),
    gen_tcp:send(Sock, "Message From Client"),
    gen_tcp:recv(Sock, 0),
    gen_tcp:close(Sock).
