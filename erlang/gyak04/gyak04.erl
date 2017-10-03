-module(gyak04).
%-export([]).
-compile(export_all).

f1() -> [math:pow(X,2) || X <- lists:seq(1, 1000), (X rem 2) == 0].

f2() -> [{A,Z} || {A, A} <- [2,{3,4},{5,5}], Z <- [5,6]].

filter([]) ->
    [];
filter([H | T]) when is_integer(H)->
    [inc(H) | filter(T)];
filter([_ | T]) ->
    filter(T).
inc(X) when is_integer(X) ->
    X + 1;
inc([]) ->
    [];
inc([H | T]) ->
    [inc(H) | inc(T) ].

map_use(L) ->
    {lists:map(fun inc/1, L),
    [inc(X) || X <- L],
    lists:map(fun(X) -> X + 1 end, L)
    }.
    
f_use(L) ->
    FL = lists:filter(fun is_integer/1, L),
    MapFL = lists:map(fun inc/1, FL),
    FMap = lists:filtermap(
        fun(X) when is_integer(X) -> {true,inc(X)};
           (_)                    -> false
        end,
        L
    ),
    LC = [inc(X) || X <- L, is_integer(X)],
    {MapFL, FMap, filter(L), LC}.
    
minl(L = [H | _]) ->
    minl(L, H).
    
minl([H], A) ->
    min(H,A);
minl([H | T], A) ->
    minl(T, min(H,A)).
    
m_use(L = [H | _]) ->
    {minl(L),
    lists:foldl(fun min/2, H, L)
    }.