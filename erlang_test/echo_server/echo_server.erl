-module(echo_server).

-export([start_server/1]).

start_server(Port) ->
    Pid = spawn_link(fun() ->
        {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
        spawn(fun() -> acceptor(Listen) end),
        timer:sleep(infinity)
    end),
    {ok, Pid}.


acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("A new peer connected.~n"),
    spawn(fun() -> acceptor(ListenSocket) end),
    handle(Socket).


handle(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"quit", _/binary>>} ->
            gen_tcp:close(Socket),
            io:format("A peer disconnected.~n");
        {tcp, Socket, Msg} ->
            gen_tcp:send(Socket, Msg),
            handle(Socket)
    end.



