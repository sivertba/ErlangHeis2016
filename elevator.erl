-module (elevator).
-compile(export_all).
-include("records_and_macros.hrl").

-export([start/0]).
-define(STATE_MONITOR, state_monitor).

% project for another day: simplify calls to self in manager

start() ->
    
    %spawn(fun() -> connection:init() end),
    timer:sleep(50),

    order_handler:start(),


	register(?STATE_MONITOR,spawn(?MODULE, state_monitor,[invalid, -1, down])),
   
	ElevatorManagerPid = spawn(fun() ->elevator_manager_init() end),
	DriverManagerPid = spawn(?MODULE, driver_manager_init, [ElevatorManagerPid]),

    ButtonLightManagerPID = spawn(fun() -> button_light_manager_init() end),

    ButtonLightManagerPID ! init_completed,
    DriverManagerPid ! init_completed.



driver_manager_init(ElevatorManagerPid) ->
	elev_driver:start(self(), elevator),
	receive
		init_completed ->
			ok
	end,
	ElevatorManagerPid ! {driver_init_completed},
	driver_manager(ElevatorManagerPid).

driver_manager(ElevatorManagerPid) ->
	receive
		{new_order, Direction, OrderFloor} ->
				order_handler:add_order(OrderFloor, Direction);
		{floor_reached, Floor} ->
			ElevatorManagerPid ! {floor_reached, Floor}
	end,
	driver_manager(ElevatorManagerPid).


elevator_manager_init() ->
	erlang:display("Elevator manager waiting for driver_init_completed"),
	% HIT MEN IKKE LENGER
	receive {driver_init_completed} ->
		ok
	end,
	FsmPid = elev_FSM:init(self()),
	erlang:display("FSM initiated"),
	receive
		{init, started} ->
			elev_driver:set_motor_direction(down)
	end,
	receive
		{floor_reached, NewFloor} ->
			elev_driver:set_floor_indicator(NewFloor),
			?STATE_MONITOR ! {update_state, floor, NewFloor},
			FsmPid ! {floor_reached}
	end,
	receive
		{init, completed} ->
			elev_driver:set_motor_direction(stop),
			?STATE_MONITOR ! {update_state, state, stationary}
	end,
	elevator_manager(FsmPid).

elevator_manager(FsmPid) ->
	receive
		{floor_reached, NewFloor} -> %from driver
			elev_driver:set_floor_indicator(NewFloor),
			?STATE_MONITOR ! {update_state, floor, NewFloor},
			?STATE_MONITOR ! {get_state, self()},
			receive
				{{_State, _Floor, LastDir}, _Node} ->
				% find if there are orders on this floor in direction of travel or commands:
				case lists:any(fun(OrderDir) -> order_handler:is_order(NewFloor, OrderDir) end, [command | LastDir]) of
					true ->
						FsmPid ! {floor_reached},
						elevator_manager(FsmPid);
					false ->
						case NewFloor of
							0 ->
								FsmPid ! {endpoint},
								elevator_manager(FsmPid);
							?NUMBER_OF_FLOORS-1 ->
								FsmPid ! {endpoint},
								elevator_manager(FsmPid);
							_ ->
								FsmPid ! {floor_passed},
								elevator_manager(FsmPid)
						end
				end
			end;
		
		% from fsm
		{awaiting_orders} ->
			Elevators = get_states(),
			Orders = order_handler:get_orders(),
			case order_distribution:action_select(Elevators,Orders) of
				wait ->
					elevator_manager(FsmPid);
				open_doors ->
					FsmPid ! {floor_reached},
					elevator_manager(FsmPid);
				Dir ->
					FsmPid ! {move, Dir},
					elevator_manager(FsmPid)
			end;
		{set_motor, Dir} ->
			elev_driver:set_motor_direction(Dir),
			case Dir of
				stop -> ?STATE_MONITOR ! {update_state, state, stationary};
				_ -> ?STATE_MONITOR ! {update_state, state, Dir}
			end,
			elevator_manager(FsmPid);
		{doors, open} ->
			elev_driver:set_door_open_lamp(on),
			elevator_manager(FsmPid);
		{doors, close} ->
			elev_driver:set_door_open_lamp(off),
			elevator_manager(FsmPid)
	end.

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

%{State, LastFloor, LastDir}, node()
state_monitor(State, LastFloor, LastDir) -> 
	receive
		{get_state, Pid} ->
			case State of
				invalid ->
					?MODULE:state_monitor(State);
				_ ->
					Pid ! {{State,LastFloor,LastDir}, node()},
					?MODULE:state_monitor(State, LastFloor, LastDir)
			end;
		{update_state, floor, NewFloor} -> 
			state_monitor(State, NewFloor,LastDir);
		{update_state, state, NewState} -> 
			state_monitor(NewState, LastFloor, LastDir);
		{update_state, dir, NewDir} -> 
			state_monitor(State, LastFloor, NewDir)
	end.

get_states() ->
	Receiver = spawn(?MODULE,merge_received,[[], self()]),
	lists:foreach(fun(Node) -> {?STATE_MONITOR, Node} ! {get_state, Receiver} end, [node()|nodes()]),
	receive
		L ->
			L
	after 100 ->
		[]
	end.

merge_received(List,Pid) -> 
	receive
		State -> 
			NewList = List ++ [State],
			MaxNumberOfStatesReceived = length(NewList) == length([node() | nodes()]),
			if % CASE
				MaxNumberOfStatesReceived ->
					Pid ! NewList;
				not MaxNumberOfStatesReceived ->
					?MODULE:merge_received(NewList, Pid)
			end
			after 50 ->
		Pid ! List
	end.

button_light_manager_init() ->
    receive init_completed ->
	    ok
    end,
    button_light_manager().
button_light_manager() ->
	Orders = order_handler:get_orders(),
    SetLightFunction = fun(Floor, Direction) ->
    	ButtonState = case order_handler:is_order(Floor, Orders) of
						 true ->
						     on;
						 false ->
						     off
					     end,
    	elev_driver:set_button_lamp(Floor, Direction, ButtonState)
		end,	 
    elev_driver:foreach_button(SetLightFunction),
    timer:sleep(200),
    button_light_manager().