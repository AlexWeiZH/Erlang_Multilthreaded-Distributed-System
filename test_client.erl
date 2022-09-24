-module(test_client).
-export([client/0]).

client() ->
    {ok, Sock} = gen_tcp:connect("localhost", 8080, [
        binary,
        {active, false},
        {packet, 2}
    ]),
    gen_tcp:send(Sock, "Message From Client"),
    io:format("~s~n", ["Client-----"]),
    A = gen_tcp:recv(Sock, 0),
    gen_tcp:close(Sock),
    A.
