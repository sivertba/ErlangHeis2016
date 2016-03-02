-module (test).
-compile(export_all).

%belongs to counter
-define (TIME_TO_NEXT, 100).

%can say something about time between orders
counter(N) -> 
	receive
		{status, Receiver} -> Receiver ! {N},counter(N);
		{reset, Receiver} -> Receiver ! {ok}, counter(0)
	after ?TIME_TO_NEXT ->
		counter(N+1)
	end.
