-module (test).
-compile(export_all).

%Kan gi info om hvor lang tid mellom hver etasje
counter(N) -> 
	receive
		{status, Pid} -> 
			Pid ! {N,counter},counter(N+1);
		{reset, Pid} -> 
			Pid ! {ok,counter}, counter(0)
	after 200 ->
		counter(N+1)
	end.

