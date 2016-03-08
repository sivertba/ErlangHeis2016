-module (order_handler).
-compile(export_all).

%Forslag til API:
-export ([	start/0,
			add_order/2,
			get_orders/0,
			get_next_order/3]).

-define(QUEUE_PID, queue).
%-define(DETS_TABLE_NAME, "orders").
-define(ORDER_EXECUTION_DEADLINE, 10000). %10 seconds

-record (order, {floor,direction, timestamp = erlang:timestamp(), node = node()}).

%Må endres
start() ->
	register(?QUEUE_PID,spawn(?MODULE,order_bank,[])).

add_order(Floor,Direction) ->
	add_order(#order{floor = Floor, direction = Direction}).
add_order(Order) when Order#order.direction =:= command -> 
	?QUEUE_PID ! {add, Order};
add_order(Order) ->
	?QUEUE_PID ! {add, Order},
	send_to_queue_on_nodes({add, Order}).

get_orders() ->
	?QUEUE_PID ! {get_orders, self()},
	receive
		L -> L
	end.

get_next_order(Last_floor, Direction,Manager) ->
	L = get_orders(),
	if	L =:= [] -> 
			?MODULE:get_next_order(Last_floor, Direction,Manager);
		L =/= [] -> 
		 	Order = order_list_filter(Last_floor,Direction,L),
		 	remove_order(Order),
		 	%the spawned functions needs to know how it went
		 	Manager ! {Order, spawn(?MODULE,order_monitor,[Order,Manager])}
	end.

remove_order(Order) -> 
	?QUEUE_PID ! {remove, Order},
	send_to_queue_on_nodes({remove, Order}).

retract_order(Order) when Order#order.direction =:= command -> 
	?QUEUE_PID ! {retract, Order};
retract_order(Order) ->
	?QUEUE_PID ! {retract, Order},
	send_to_queue_on_nodes({retract, Order}).

order_monitor(Order,Manager) ->
	receive
		{order_handeld} -> 
			remove_order(Order);
		{order_aborted} -> 
			retract_order(Order)
	after ?ORDER_EXECUTION_DEADLINE -> 
		Manager ! timeout,
		retract_order(Order)
	end.

%Burde skrives om til å lagre på disk (dets)
order_bank() ->
	order_bank([]).
order_bank(L) ->
	receive
		{get_orders, Pid} ->
			Pid ! L,
			?MODULE:order_bank(remove_duplicates(L));
		{add, Order} -> 
			NewL = lists:append(L,[Order]),
			?MODULE:order_bank(remove_duplicates(NewL));
		{remove, Order} -> 
			?MODULE:order_bank(lists:delete(Order, L));
		{retract, Order} ->
			NewL = lists:append([Order],L),
			?MODULE:order_bank(remove_duplicates(NewL))
	end.


remove_duplicates(L) ->
	%Må se bort i fra node og tid
	L.	
order_list_filter(Last_floor,Direction,L) ->
	PossibleOrders = lists:filter(fun(Elem) -> on_path(Elem,Last_floor,Direction) end, L),
	SortedOrders = 	ordsets:from_list(PossibleOrders),
	if
		Direction =:= up ->
			lists:nth(1,SortedOrders);
		Direction =:= down ->
			lists:last(SortedOrders);
		true -> %else
			lists:nth(1,SortedOrders)
	end.

on_path(#order{floor=Floor,direction=Dir},Last_floor, Direction) ->
	if 	((Dir =:= up) or (Dir =:= command)) and (Direction =:= up) ->
			if
				Last_floor > Floor -> 
					false;
				Last_floor =< Floor -> 
					true
			end;
		((Dir =:= down) or (Dir =:= command)) and (Direction =:= down) ->
			if
				Last_floor < Floor -> 
					false;
				Last_floor >= Floor-> 
					true
			end;
		Direction =:= command -> 
			true;
		true -> 
			false
	end.

send_to_queue_on_nodes(Msg) ->
	lists:foreach(fun(Node) -> {?QUEUE_PID, Node} ! Msg end,nodes()).
