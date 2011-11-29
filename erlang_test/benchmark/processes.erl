%%#! /usr/bin/env escript

-module(processes).

-export([create/1, create_and_destroy/1]).

%% max(N)
%%   Create N processes then destry them
%%   See how much time this takes

create_and_destroy(N) ->
    Max = erlang:system_info(process_limit),
    io:format("Maximum allowed processes:~p~n", [Max]),
    statistics(runtime),
    statistics(wall_clock),
    L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! die end, L),
    U1 = Time1 * 1000/N,
    U2 = Time2 * 1000/N,
    io:format("Process spawn time=~p (~p) microseconds~n", [U1, U2]).

create(N) ->
    Max = erlang:system_info(process_limit),
    io:format("Maximum allowed processes:~p~n", [Max]),
    statistics(runtime),
    statistics(wall_clock),
    L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    % Without destroy them
    % lists:foreach(fun(Pid) -> Pid ! die end, L),
    U1 = Time1 * 1000/N,
    U2 = Time2 * 1000/N,
    io:format("Process spawn time=~p (~p) microseconds~n", [U1, U2]).

wait() ->
    receive
        die -> void
    end.


main([String]) ->
    try
        N = list_to_integer(String),
        create(N)
    catch
        _:_ ->
            io:format("usage: process.erl num_of_processes\n")
    end;

main(_) ->
    io:format("usage: process.erl num_of_processes\n").


for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].
