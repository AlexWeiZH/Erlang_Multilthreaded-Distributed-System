-module(server).
-import(io, [fwrite/1]).
-export([start/1, get_random_string/0, conv_to_hash/1, mining/1, continue/1, server/1]).
-define(AllowedChars,
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz;./`!@#$%^&*()1234567890"
).
-define(Ufid, "r.zhu").

start(Number) ->
    RandomPID = spawn(?MODULE, get_random_string, []),
    register(p_random, RandomPID),
    HashPID = spawn(?MODULE, conv_to_hash, ["r.zhu"]),
    register(p_hash, HashPID),
    beforeMining(Number),

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
    io:format("~s~n", ["server mining..."]),
    receive
        {tcp, Socket, Data} ->
            % Receive data from client
            io:format("~s~n~s", ["Mining request received by server", binary_to_list(Data)]),
            % Default the mine the coins start with 5 "0"s
            beforeMining(5),
            gen_tcp:send(Socket, "Mining completed"),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Socket ~w closed [~w]~n", [Socket, self()]),
            ok
    end.
% Generate a radom string
get_random_string() ->
    receive
        {TargetPID, Length, covertTohash} ->
            %String generated
            RandomString = lists:foldl(
                fun(_, Acc) ->
                    [lists:nth(rand:uniform(length(?AllowedChars)), ?AllowedChars)] ++ Acc
                end,
                [],
                lists:seq(1, Length)
            ),
            %Concante with UFID
            ConcantedString = string:concat(?Ufid, RandomString),
            %io:format("~s~s~n", ["ConcantedString:", ConcantedString]),
            %Send message back
            TargetPID ! {self(), ConcantedString, random}
    end,
    get_random_string().

conv_to_hash(Hash) ->
    receive
        %Receive generated string
        {Ran_pid, ConcantedString, random} ->
            %Hash the string
            %io:format("~s~s~n", ["Hash:", ConcantedString]),
            conv_to_hash(ConcantedString);
        %Receive mining request
        {Minning_PID, mining} ->
            %Contact get_random_string to get the generated string
            p_random ! {self(), 30, covertTohash},
            Bitcoin = io_lib:format("~64.16.0b", [
                binary:decode_unsigned(crypto:hash(sha256, Hash))
            ]),
            %Send mining hash result back
            Minning_PID ! {self(), Hash, Bitcoin, covertTohash},
            %io:format("~s~s~n", ["Bitcoin:", Bitcoin]),
            conv_to_hash(Hash)
    end.

beforeMining(Number) ->
    %statistics(runtime),
    %statistics(wall_clock),
    spawn(?MODULE, mining, [Number]).

mining(ZeroNumber) ->
    p_hash ! {self(), mining},
    receive
        {HashedPID, Hash, Bitcoin, covertTohash} ->
            % "000"
            Zeros = generate_0_string(ZeroNumber),
            % "0sd"
            Substring = string:sub_string(Bitcoin, 1, ZeroNumber),
            if
                Zeros == Substring ->
                    io:format("~n~w~n~s~s~s~n", [ZeroNumber, Hash, "\t", Bitcoin]);
                %{_, Time1} = statistics(runtime),
                %{_, Time2} = statistics(wall_clock),
                %Ratio = Time1 / Time2,
                %io:format("~s~f~n", ["ratio is : ", Ratio]);
                true ->
                    mining(ZeroNumber)
            end
    end.

continue(Number1) ->
    beforeMining(Number1).

generate_0_string(0) ->
    "";
generate_0_string(ZeroNumber) ->
    "0" ++ generate_0_string(ZeroNumber - 1).
