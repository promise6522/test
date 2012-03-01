-module(tcp_server).

-export([start/0]).

start() ->
    {ok, Listen} = gen_tcp:listen(8888, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            % client would send {atom, int}
            io:format("Server received binary = ~p~n", [Bin]),
            {client, Number} = binary_to_term(Bin),

            % response with {N+1, server}
            gen_tcp:send(Socket, term_to_binary({Number+1, server})),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.
