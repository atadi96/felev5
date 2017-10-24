-module(processes).
-export([run/1, ping/0]).
-export([map/2, parmap/2, ordparmap/2, ordparmap2/2]).
% pid!msg <- aszinkron üzenetküldés
% receive <- üzenet fogadása, mintaillesztéssel, blokkol

map(F,L) ->
	[F(E) || E <- L],
	[apply(F, E) || E <- L].

parmap(F,L) ->
	Parent = self(),
	[spawn(fun() -> Parent ! F(E) end) || E <- L],
	[receive
		A -> A
	end || _ <- L].
	
ordparmap(F,L) ->
	Parent = self(),
	[spawn(fun() -> Parent ! {E, F(E)} end) || E <- L],
	[receive
		{E, Result} -> Result
	end || E <- L].
	
ordparmap2(F,L) ->
	Parent = self(),
	Pids = [spawn(fun() -> Parent ! {self(), F(E)} end) || E <- L],
	[receive
		{P, Result} -> Result
	end || P <- Pids].
	
ping() ->
	receive
		{ping, From} ->
			From ! {pong, self()},
			ping()
	end.

% Pid = spawn(processes, ping, []).
	
run(Pid) ->
	Pid ! {ping, self()},
	receive
		{pong, Pid} ->
			io:format("~p is alive~n", [Pid]),
			ping;
		A ->
			io:format("other message: ~p~n", [A])
	after 5000 ->
		pong
	end.