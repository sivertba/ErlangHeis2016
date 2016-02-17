-module (ex6).
-compile(export_all).

%Counter

start() ->
    register(backup,spawn(backup())).

counter1() ->
	counter1(0).
counter1(N) ->
    Backup ! {ok, N},
	io:format("~p~n",[N]),
	timer:sleep(500),
	counter1(N+1).

backup() ->
    receive 
        {ok, N} ->
            % ???
    after 5000 -> 
        ok
    end.
