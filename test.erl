-module (test).
-compile(export_all).

%ElvatorType is simulator or 
start(ElevatorType) ->
	register(queue,spawn(order_handler,order_bank,[])),
	connection:init(),

	DriverManagerPID = spawn(fun() -> driver_manager_init() end),
    elev_driver:start(DriverManagerPID, ElevatorType),

    ButtonLightManagerPID = spawn(fun() -> button_light_manager_init() end),

  	DriverManagerPID ! init_completed,
  	ButtonLightManagerPID ! init_completed.


driver_manager_init() ->
    receive init_completed ->
	    ok
    end,
    driver_manager().
driver_manager() ->
    receive
	{new_order, Direction, Floor} ->
	    order_distributer:add_order(Floor, Direction);
	{floor_reached, Floor} ->
	    elev_driver:set_floor_indicator(Floor),
	    elev_fsm:event_floor_reached(fsm),
	    schedule:floor_reached(schedule, Floor)
    end,
    driver_manager().

%kanskje gjøre dette til en egen prosess i dirveren.
button_light_manager_init() ->
    receive init_completed ->
	    ok
    end,
    button_light_manager().
button_light_manager() ->
    SetLightFunction = fun(Floor, Direction) ->
			       ButtonState = case order_distributer:is_order(Floor, Direction) of
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