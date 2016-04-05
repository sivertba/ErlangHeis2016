-module (elevator).
-compile(export_all).
-include("orders.hrl").

-export([start/0]).


start() ->
    connection:init(),
    timer:sleep(50),

    order_handler:start(),
    
    DriverManagerPID = spawn(fun() -> driver_manager_init() end),
    elev_driver:start(DriverManagerPID, elevator),
    
    ButtonLightManagerPID = spawn(fun() -> button_light_manager_init() end),

    FsmManagerPID = spawn(fun() -> fsm_manager_init() end),
    FsmPID = elev_FSM:init(FsmManagerPID),
    register(fsm, FsmPID),

    DriverManagerPID ! init_completed,
    ButtonLightManagerPID ! init_completed.

fsm_manager_init() ->
    receive init_completed ->
	    ok
    end,
    fsm_manager(-1).
fsm_manager(Floor) ->
    receive
		{init, started} ->
	    	elev_driver:set_motor_direction(down);
	    {init, completed, Floor} ->
	    	ok;
	    {motor, stop} ->
	    	elev_driver:set_motor_direction(stop);
	    {getdir} -> %her blir det hårete ...
	    	%costfunction regner ut hvor vi burde dra
	    	Bool = false, %må teste om vi er i topp eller bunn
	    	if
	    		Bool ->
	    			ok
	    	end;
	   	{doors, open} -> ok;
	   	{doors, closed} -> ok;
	   end,
	   fsm_manager(Floor).


driver_manager_init() ->
    receive init_completed ->
	    ok
    end,
    driver_manager().
driver_manager() ->
    receive
	{new_order, Direction, Floor} ->
	    order_handler:add_order(Floor, Direction);
	{floor_reached, Floor} ->
	    elev_driver:set_floor_indicator(Floor),
	    fsm ! new_order
	    elev_fsm:event_floor_reached(fsm),
    end,
    driver_manager().

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