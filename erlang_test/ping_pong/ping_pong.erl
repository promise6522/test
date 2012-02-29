-module(ping_pong).

-export([main/1]).

% Compile before run to get proper results
-mode(compile).

-define(MAX_ROUND, 10000000).

start_ping() ->
    Pong = spawn(fun() -> pong() end),
    Pong ! {ping, self(), 0},
    ping(0).


ping(N) ->
    receive
        {pong, Pong, ?MAX_ROUND} ->
            io:format("~p round trips finished.~n", [?MAX_ROUND]),
            Pong ! {finished},
            main ! {finished, ?MAX_ROUND};

        {pong, Pong, N} ->
            Next = N + 1,
            Pong ! {ping, self(), Next},
            ping(Next)
    end.

pong() ->
    receive
        {finished} ->
            io:format("Pong finished.~n");
        {ping, Ping, N} ->
            Ping ! {pong, self(), N},
            pong()
    end.

main(_) ->
    % register the main process
    register(main, self()),

    statistics(runtime),
    statistics(wall_clock),

    spawn(fun() -> start_ping() end),
    receive
        {finished, N} ->
            {_, Time1} = statistics(runtime),
            {_, Time2} = statistics(wall_clock),
            U1 = Time1 * 1000,
            U2 = Time2 * 1000,
            io:format("~p round trips in ~p (~p) us. ~n", [N, U1, U2]),
            io:format("Average round-trip time : ~p us ~n", [U1/N])
    end.
