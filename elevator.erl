-module (elevator).
-compile(export_all).
-include("orders.hrl").

-export([start/0]).
-define(STATE_MONITOR, state_monitor).

start() ->
    
    spawn(fun() -> connection:init() end),
    timer:sleep(50),

    order_handler:start(),
   
	ElevatorManagerPid = spawn(fun() ->elevator_manager_init() end),   

    ButtonLightManagerPID = spawn(fun() -> button_light_manager_init() end),

    register(?STATE_MONITOR,spawn(?MODULE, state_monitor,[])).

    ButtonLightManagerPID ! init_completed,
    ElevatorManagerPid ! init_completed.


state_monitor() -> 
	receive
		get_state -> State;
		update_state -> state_monitor()
	end.

get_states() ->
	F = fun(E) ->
		{?STATE_MONITOR, E} ! get_state
		receive
			State -> {State, E}
		end
	end,
	lists:map(F,[node()|nodes()]).


elevator_manager_init() ->
	FsmPid = elev_FSM:init(self()),
	elev_driver:start(self(),elevator),
	receive
		init_completed ->
			ok
	end,
	elevator_manager(FsmPid,-1).

elevator_manager(FsmPid, Floor) ->
	receive
	{new_order, Direction, OrderFloor} ->
		order_handler:add_order(OrderFloor, Direction);
	{floor_reached, NewFloor} ->
		elev_driver:set_floor_indicator(NewFloor),
		case lists:any(fun(Dir) -> order_handler:is_duplicates(#order{floor = NewFloor, direction = Dir}, order_handler:get_orders()) end, [command]) of %husk a tilfoye | getdir
			true ->
				FsmPid ! {floor_reached, NewFloor},
				elevator_manager(FsmPid, NewFloor);
			false ->
				case NewFloor of
					0 orelse 3 ->
						FsmPid ! {endpoint, NewFloor},
						elevator_manager(FsmPid, NewFloor);
					1 orelse 2 ->
						elevator_manager(FsmPid, NewFloor)
				end
		end;
	{init, started} ->
		elev_driver:set_motor_direction(down),
		elevator_manager(FsmPid, Floor);
	{init, completed, NewFloor} ->
		elev_driver:set_floor_indicator(NewFloor),
		elevator_manager(FsmPid, NewFloor);
	{idle} ->
		elev_driver:set_motor_direction(stop),
		case order_distribution:direction_picker() of
			open_doors ->
				FsmPid ! {floor_reached, Floor},
				elevator_manager(FsmPid, Floor);
			Dir ->
				elev_driver:set_motor_direction(Dir),
				elevator_manager(FsmPid, Floor)
		end;
	{doors, open} ->
		elev_driver:set_motor_direction(stop),
		elev_driver:set_door_open_lamp(on),
		elevator_manager(FsmPid, Floor);
	{doors, close} ->
		elev_driver:set_door_open_lamp(off),
		elevator_manager(FsmPid, Floor);
	end,
	elevator_manager(FsmPid, Floor).



%fsm_manager_init() ->
%	FsmPID = elev_FSM:init(self()),
%    receive init_completed ->
%	    ok
%    end,
%    fsm_manager(FsmPID,-1).
%fsm_manager(FsmPID,Floor) ->
%    receive
%		{init, started} ->
%	    	drivermanager ! {motor,down}
%	    {init, completed, Floor} ->
%	    	ok;
%	    {motor, stop} ->
%	    	elev_driver:set_motor_direction(stop);
%	    {getdir} -> %her blir det hårete ...
%	    	%costfunction regner ut hvor vi burde dra
%	    	Bool = false, %må teste om vi er i topp eller bunn
%	    	if
%	    		Bool ->
%	    			ok
%	    	end;
%	   	{doors, open} -> ok;
%	   	{doors, closed} -> ok;
%	   end,
%	   fsm_manager(FsmPID,Floor).


%driver_manager_init() ->
%	elev_driver:start(drivermanager, elevator),
%    receive init_completed ->
%	    ok
%    end,
%    driver_manager().
%driver_manager() ->
%    receive
%	{new_order, Direction, Floor} ->
%	    order_handler:add_order(Floor, Direction);
%	{floor_reached, Floor} ->
%	    elev_driver:set_floor_indicator(Floor),
%	    elev_fsm:event_floor_reached(fsm);
%	{motor, Dir} ->
%		elev_driver:set_motor_direction(Dir)
%    end,
%    driver_manager().

button_light_manager_init() ->
    receive init_completed ->
	    ok
    end,
    button_light_manager().
button_light_manager() ->
	Orders = order_handler:get_orders(),
	ButtonState = case order_handler:is_duplicates(#order{floor = Floor, direction = Direction},Orders) of
						 true ->
						     on;
						 false ->
						     off
					     end,
    SetLightFunction = fun(Floor, Direction) ->
    			elev_driver:set_button_lamp(Floor, Direction, ButtonState)
				end,	 
    elev_driver:foreach_button(SetLightFunction),
    timer:sleep(200),
    button_light_manager().