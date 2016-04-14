-module (elev_FSM).
-include("records_and_macros.hrl").
-compile(export_all).
-export([init/1]).

init(Manager) ->
	Manager ! {init, started},
	receive 
		{floor_reached} ->
			Manager ! {init, completed}
	end,
	idle(Manager).

idle(Manager) ->
	Manager ! {awaiting_orders},
	receive
		{move, Dir} ->
			Manager ! {set_motor, Dir},
			moving(Manager);
		{floor_reached} -> 
			doors_open(Manager)
	after 200 ->
		idle(Manager)
	end.

doors_open(Manager) ->
	Manager ! {doors, open},
	timer:sleep(?DOOR_OPEN_DURATION),
	Manager ! {doors, close},
	idle(Manager).


moving(Manager) -> 
	receive
		{floor_passed} ->
			moving(Manager);
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