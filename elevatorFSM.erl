-module (elevatorFSM).
%-compile(export_all).
-behaviour (gen_fsm).


-define (DOOR_OPEN_TIME, 3000).



motorUp(Pid) -> Pid ! {motor,up}.
motorDown(Pid) -> Pid ! {motor,down}.
motorStop(Pid) -> Pid ! {motor,stop}.
openDoor(Pid) -> Pid ! {door, open}.
closeDoor(Pid) -> Pid ! {door,close}.
initStarted(Pid) -> Pid ! {init, started}.
initCompleted(Pid) -> Pid ! {init,complete}.

newDirectionRequest(Pid) ->
	Pid ! {direction, request, self()},
	receive
		{direction, respons, Direction} ->
			Direction
	end.

start(Pid) -> 
	spawn(fun() -> s_init(Pid) end).

% s_STATENME

s_init(Pid) ->
	flush(),
	initStarted(Pid),
	receive
		arrivedAtFloor ->
			initCompleted(Pid),
			s_stationary(Pid)
	end.

s_movingUp(Pid) ->
	flush(),
	motorUp(Pid),
	receive
		arrivedAtFloor ->
			s_stationary(Pid)
	end.
	
s_movingDown(Pid) ->
	flush(),
	motorDown(Pid),
	receive
		arrivedAtFloor ->
			s_stationary(Pid)
	end.

s_stationary(Pid) ->
	flush(), 
	motorStop(Pid),
	NewDirection = newDirectionRequest(Pid),
	case NewDirection of
		up 	-> s_movingUp(Pid);
		down->s_movingDown(Pid);
		open->s_openDoor(Pid);
		stop->
			receive
				newOrder ->
					s_stationary(Pid).
			end.
	end.

s_openDoor(Pid) ->
	flush(),
	openDoor(Pid),
	timer:sleep(?DOOR_OPEN_TIME),
	closeDoor(Pid),
	s_stationary(Pid).


flush() ->
    receive _Any ->
	    flush()
    after 0 ->
	    ok
    end.