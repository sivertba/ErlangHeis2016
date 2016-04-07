-module (elev_FSM).
%-export ().
%-compile(export_all).
%gen_fsm Callbacks
-export([  	init/1,
         	idle/1,
         	doors_open/1,
         	moving/1]).

-define (DOOR_OPEN_DURATION, 3000).

init(Manager) ->
	flush(),
	Manager ! {init, started},
	receive 
		{floor_reached, Floor} ->
			Manager ! {init, completed, Floor},
			idle(Manager)
	end.

idle(Manager) ->
	flush(),
	Manager ! {idle},
	receive
		{move, Dir} ->
			Manager ! {motor, Dir},
			moving(Manager);
		{floor_reached, Floor} -> 
			Manager ! {arrived, Floor},
			doors_open(Manager)
	end.

doors_open(Manager) ->
	flush(),
	Manager ! {open, doors},
	timer:sleep(?DOOR_OPEN_DURATION),
	Manager ! {doors, close},
	idle(Manager).


moving_up(Manager) -> 
	flush(),
	receive
		{floor_reached, Floor} ->
			Manager ! {arrived, Floor},
			doors_open(Manager);
		{endpoint, _Floor} ->
			idle(Manager)
	after 6000 ->
		stuck(Manager)
			% if for some reason the elevator reaches an endpoint 
			% where there are no orders, just stop.
	end.
		
moving_down(Manager) -> 
	flush(),
	receive
		{floor_reached, Floor} ->
			Manager ! {arrived, Floor},
			doors_open(Manager);
		{endpoint, _Floor} ->
			idle(Manager)
		after 6000 ->
			stuck(Manager)
			% if for some reason the elevator reaches an endpoint 
			% where there are no orders, just stop.
	end.

stuck(Manager)->
	flush(),
	receive
		{} ->


flush() ->
    receive _Any ->
	    flush()
    after 0 ->
	    ok
    end.