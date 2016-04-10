-module (elev_FSM).
-compile(export_all).
-export([init/1]).

-define (DOOR_OPEN_DURATION, 3000).
-define (EXPECTED_MAX_TIME_BETWEEN_FLOORS, 7000).

% if we are ever bored: go back to a single moving state


init(Manager) ->
	%flush(),
	Manager ! {init, started},
	receive 
		{floor_reached} ->
			Manager ! {init, completed}
	end,
	idle(Manager).

idle(Manager) ->
	%flush(),
	Manager ! {awaiting_orders},
	receive
		{move, up} ->
			Manager ! {set_motor, up},
			moving_up(Manager);
		{move, down} ->
			Manager ! {set_motor, down},
			moving_down(Manager);
		{floor_reached} -> 
			doors_open(Manager)
	after 200 ->
		idle(Manager)
	end.

doors_open(Manager) ->
	%flush(),
	Manager ! {doors, open},
	timer:sleep(?DOOR_OPEN_DURATION),
	Manager ! {doors, close},
	idle(Manager).


moving_up(Manager) -> 
	%flush(),
	receive
		{floor_passed} ->
			moving_up(Manager);
		{floor_reached} ->
			Manager ! {set_motor, stop},
			doors_open(Manager);
		{endpoint} ->
			Manager ! {set_motor, stop},
			idle(Manager)
	after ?EXPECTED_MAX_TIME_BETWEEN_FLOORS ->
		stuck(Manager)
	end.
		
moving_down(Manager) -> 
	%flush(),
	receive
		{floor_passed} ->
			moving_down(Manager);
		{floor_reached} ->
			Manager ! {set_motor, stop},
			doors_open(Manager);
		{endpoint} ->
			Manager ! {set_motor, stop},
			idle(Manager)
		after ?EXPECTED_MAX_TIME_BETWEEN_FLOORS ->
			stuck(Manager)
	end.

stuck(Manager)->
	%flush(),
	Manager ! {stuck},
	receive
		{floor_reached} ->
			Manager ! {set_motor, stop},
			idle(Manager);
		{floor_passed} ->
			Manager ! {set_motor, stop},
			idle(Manager);
		{endpoint} ->
			Manager ! {set_motor, stop},
			idle(Manager)
	end.



flush() ->
    receive _Any ->
	    flush()
    after 0 ->
	    ok
    end.