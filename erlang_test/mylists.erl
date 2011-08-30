-module(mylists).
-export([map/2]).
-export([seq/2]).

map(_, [])      -> [];
map(F, [H|T])   -> [F(H)|map(F, T)].

seq(Max, Max)   -> [Max];
seq(Min, Max)   -> [Min|seq(Min+1, Max)].
