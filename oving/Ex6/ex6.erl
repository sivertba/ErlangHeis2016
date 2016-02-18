-module (ex6).
-export ([start/0]).

%attempted process

start() ->
	spawn(fun() -> counter1() end).

counter1() ->
	random:seed(erlang:system_time(nano_seconds)),
	DeathNumber = random:uniform(100),
	io:format("This is the number i will die at: ~p~n",[DeathNumber]),
	Backup = spawn(fun() -> backup() end),
	counter1(0, Backup, DeathNumber).

counter1(N, Backup, DeathNumber) ->
	if 
	N < DeathNumber ->
	Backup ! {ok, N, DeathNumber},
		io:format("~p~n",[N]),
		timer:sleep(100),
		counter1(N+1, Backup, DeathNumber);
	N >= DeathNumber ->
		io:format("~p~n",[N])
	end.


backup() ->
    receive 
        {ok, _N, _DeathNumber} ->
            backup()
    after 3000 ->
        counter1()
    end.
