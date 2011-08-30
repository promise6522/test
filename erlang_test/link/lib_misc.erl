-module(lib_misc).
-export([on_exit/2]).

on_exit(Pid, Fun) ->
    spawn(fun() -> 
                process_flag(trap_exit, true),
                link(Pid),
                receive
                    {'EXIT', Pid, Why} -> 
                        Fun(Why)
                end
        end).

