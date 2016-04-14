-module (elevator).
-compile(export_all).
-include("records_and_macros.hrl").

-export([start/0]).

start() ->
    
    order_handler:start(),
    timer:sleep(50),
    spawn(fun() -> connection:init() end),
    timer:sleep(50),

	register(?STATE_MONITOR, spawn(?MODULE, state_monitor,[invalid, -1, down])),
   
	ElevatorManagerPid = spawn(fun() ->elevator_manager_init() end),
	
	register(?FSM_PID, spawn(fun() -> elev_FSM:init(ElevatorManagerPid) end)),

	ButtonLightManagerPID = spawn(fun() -> button_light_manager_init() end),

	DriverManagerPid = spawn(?MODULE, driver_manager_init, [ElevatorManagerPid]),

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
		{new_order, Direction, Floor} ->
			order_handler:add_order(Floor, Direction);
		{floor_reached, Floor} ->
			elev_driver:set_floor_indicator(Floor),
			ElevatorManagerPid ! {floor_reached, Floor}
	end,
	driver_manager(ElevatorManagerPid).


elevator_manager_init() ->
	receive {driver_init_completed} ->
		ok
	end,
	receive
		{init, started} ->
			elev_driver:set_motor_direction(down)
	end,
	receive
		{floor_reached, NewFloor} ->
			elev_driver:set_floor_indicator(NewFloor),
			?STATE_MONITOR ! {update_state, floor, NewFloor},
			?FSM_PID ! {floor_reached}
	end,
	receive
		{init, completed} ->
			elev_driver:set_motor_direction(stop),
			?STATE_MONITOR ! {update_state, state, stationary}
		end,
	elevator_manager().

elevator_manager() ->
	receive
		%from driver
		{floor_reached, NewFloor} -> 
			elev_driver:set_floor_indicator(NewFloor),
			?STATE_MONITOR ! {update_state, floor, NewFloor},
			?STATE_MONITOR ! {get_state, self()},
			receive
				{{_State, _Floor, LastDir}, _Node} ->
					ToRemove = order_handler:to_remove(NewFloor, LastDir),
					case ToRemove of
						{[], true} when NewFloor /= 0 andalso NewFloor /= ?NUMBER_OF_FLOORS-1->
							?FSM_PID ! {floor_passed},
							elevator_manager();
						{[], false} ->
							?FSM_PID ! {endpoint},
							elevator_manager();
						{ToRemoveList, _} ->
							?FSM_PID ! {floor_reached},
							lists:foreach(fun(E) -> order_handler:remove_order(E) end, ToRemoveList),
							elevator_manager()
					end
			end;

		% from FSM
		{awaiting_orders} ->
			Elevators = get_states(),
			Orders = order_handler:get_orders(),
			Action = order_distribution:action_select(Elevators,Orders),
			case Action of
				wait ->
					elevator_manager();
				open_doors ->
					?FSM_PID ! {floor_reached},
					{{_State, Floor, Dir}, _Node} = lists:keyfind(node(), 2, Elevators),
					{ToRemove, _} = order_handler:to_remove(Floor, Dir),
					lists:foreach(fun(E) -> order_handler:remove_order(E) end, ToRemove),
					elevator_manager();
				Dir ->
					?FSM_PID ! {move, Dir},
					elevator_manager()
			end;
		{set_motor, Dir} ->
			elev_driver:set_motor_direction(Dir),
			case Dir of
				stop -> ?STATE_MONITOR ! {update_state, state, stationary};
				_ -> 
					?STATE_MONITOR ! {update_state, state, moving},
					?STATE_MONITOR ! {update_state, dir, Dir}
			end,
			elevator_manager();
		{doors, open} ->
			elev_driver:set_door_open_lamp(on),
			elevator_manager();
		{doors, close} ->
			elev_driver:set_door_open_lamp(off),
			elevator_manager();
		{stuck} ->
			?STATE_MONITOR ! {update_state, state, stuck},
			elevator_manager()
	end.

state_monitor(State, LastFloor, LastDir) -> 
	receive
		{get_state, Pid} ->
			case State of
				invalid ->
					?MODULE:state_monitor(State, LastFloor, LastDir);
				_ ->
					Pid ! {{State,LastFloor,LastDir}, node()},
					?MODULE:state_monitor(State, LastFloor, LastDir)
			end;
		{update_state, floor, NewFloor} ->
			case NewFloor of
				0 -> state_monitor(State, NewFloor, up);
				?NUMBER_OF_FLOORS-1 -> state_monitor(State, NewFloor, down);
				_ -> state_monitor(State, NewFloor, LastDir)
			end; 
		{update_state, state, NewState} -> 
			state_monitor(NewState, LastFloor, LastDir);
		{update_state, dir, NewDir} -> 
			state_monitor(State, LastFloor, NewDir)
	end.

%returns the sate of all the elevators in a list
get_states() ->
	Receiver = spawn(?MODULE,merge_received,[[], self()]),
	lists:foreach(fun(Node) -> {?STATE_MONITOR, Node} ! {get_state, Receiver} end, [node()|nodes()]),
	receive
		L ->
			L
	after 100 ->
		[]
	end.

%Makes a list of every connected elevator's state and passes it on
merge_received(List,Pid) -> 
	receive
		State -> 
			NewList = List ++ [State],
			MaxNumberOfStatesReceived = length(NewList) == length([node() | nodes()]),
			case MaxNumberOfStatesReceived of
				true ->
					Pid ! NewList;
				false ->
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
    SetLightFunction = fun(Floor, Direction) ->
			       ButtonState = case order_handler:is_order(Floor, Direction) of
						 true ->
						     on;
						 false ->
						     off
					     end,
			       elev_driver:set_button_lamp(Floor, Direction, ButtonState)
		       end,	  
    elev_driver:foreach_button(SetLightFunction),
    timer:sleep(100),
    button_light_manager().
