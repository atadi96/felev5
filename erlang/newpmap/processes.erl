-module(processes).
-export([ordparmap2/2, newpmap1/3, newpmap2/3, test1/0, test2/0]).

sfib(1) -> 0;
sfib(2) -> 1;
sfib(N) -> sfib(N-2) + sfib(N-1).
    
ordparmap2(F,L) ->
    Parent = self(),
    Pids = [spawn(fun() -> Parent ! {self(), F(E)} end) || E <- L],
    [receive
        {P, Result} -> Result
    end || P <- Pids].

partition(List,PartNum) ->
    Length = length(List),
    LongerPartNum = Length rem PartNum,
    ShorterPartNum = PartNum - LongerPartNum,
    ShorterLength = Length div PartNum,
    partition(List,LongerPartNum,ShorterPartNum,ShorterLength,[]).
partition(_List, 0, 0, _ShortLength, Result) ->
    Result;
partition(List, 0, ShortNum, ShortLength, Result) ->
    {F,S} = lists:split(ShortLength, List),
    partition(S, 0, ShortNum-1, ShortLength, Result++[F]);
partition(List, LongNum, ShortNum, ShortLength, Result) ->
    {F,S} = lists:split(ShortLength+1, List),
    partition(S, LongNum-1, ShortNum, ShortLength, Result++[F]).

newpmap1(F,List,ProcessNum) ->
    Parent = self(),
    SubLists = partition(List, ProcessNum),
    Tasks =
        [ spawn(
            fun() -> Parent ! {self(), lists:map(F, SubList)} end
        ) || SubList <- SubLists ],
    Results =
        [ receive
            {Task, Result} -> Result
        end || Task <- Tasks ],
    lists:flatten(Results).

child_process(F,Parent) ->
    receive
        {data,X} ->
            Parent ! {self(), F(X)},
            child_process(F,Parent);
        finish ->
            ok
    end.

threads_with(F,Num) ->
    [spawn(F) || _ <- lists:seq(1,Num)].

newpmap2(F,List,ProcessNum) ->
    Self = self(),
    Threads = threads_with(fun() -> child_process(F,Self) end, ProcessNum),
    Results = newpmap2(List,[],Threads,[]),
    Results.
newpmap2([],[],[],Result) ->
    Result;
newpmap2([],[],[Free | Frees],Result) ->
    Free ! finish,
    newpmap2([],[],Frees,Result);
newpmap2([],[Working | Ws],Frees,Result) ->
    receive
        {Working, X} -> newpmap2([],Ws,Frees++[Working],Result++[X])
    end;
newpmap2([In | Put],Working,[Free | Frees],Result) ->
    Free ! {data,In},
    newpmap2(Put,Working++[Free],Frees,Result);
newpmap2([In | Put],[Working | WS],[],Result) ->
    receive
        {Working, X} ->
            Working ! {data,In},
            newpmap2(Put,WS++[Working],[],Result++[X])
    end.

test1() ->
    ordparmap2(fun sfib/1, [35,36,37,38]) == newpmap1(fun sfib/1, [35,36,37,38], 8).

test2() ->
    ordparmap2(fun sfib/1, [35,36,37,38]) == newpmap2(fun sfib/1, [35,36,37,38], 8).
