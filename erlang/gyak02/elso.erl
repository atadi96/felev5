-module(elso).
-export([add/2, gcd/2]). % export "add" function with two parameters, in a list

add(X, Y) -> % variable names start with capital letters
    X + Y.

gcd(X, Y) when X > Y ->
    io:format("x = ~p, y = ~p", [X, Y]),
    gcd(X-Y, Y);
gcd(X, Y) when Y > X ->
    gcd(X, Y-X);
% gcd(X, Y) when X = Y -> X
gcd(X, X) -> X. % it's allowed, it's not hiding, but checking fo equality
