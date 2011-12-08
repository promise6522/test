-module(geometry).
-export([area/1]).
area({rectangle, Width, Ht}) -> Width * Ht;
area({circle, R}) -> 
    timer:sleep(10000),
    3.14159 * R * R.
