-module (order_handler).
-compile(export_all).

%Forslag til API:
-export ([start/0,add_order/2,get_next_order/3]).

-define(ORDER_EXECUTION_DEADLINE, 5000). %5 seconds
-define (QUEUE_PID, queue).

-record (order, {floor,direction}).

start() ->
	register(queue,spawn(?MODULE,order_bank,[])).

add_order(Floor,Direction) ->
	add_order(#order{floor = Floor, direction = Direction}).
add_order(Order) when Order#order.direction =:= idle -> 
	?QUEUE_PID ! {add, Order};
add_order(Order) ->
	?QUEUE_PID ! {add, Order},
	send_to_queue_on_nodes({add, Order}).

remove_order(Order) -> 
	?QUEUE_PID ! {remove, Order},
	send_to_queue_on_nodes({remove, Order}).

retract_order(Order) when Order#order.direction =:= idle -> 
	?QUEUE_PID ! {retract, Order};
retract_order(Order) ->
	?QUEUE_PID ! {retract, Order},
	send_to_queue_on_nodes({retract, Order}).

get_next_order(Last_floor, Direction,Manager) ->
	L = get_orders(),
	if	L =:= [] -> 
			?MODULE:get_next_order(Last_floor, Direction,Manager);
		true -> 
		 	Order = order_list_filter(Last_floor,Direction,L),
		 	remove_order(Order),
		 	%the spawned functions needs to know how it went
		 	Manager ! {Order, spawn(?MODULE,order_monitor,[Order,Manager])}
	end.
	
order_monitor(Order,Manager) ->
	receive
		{order_handeld} -> ok;
		%found something better
		{order_aborted} -> retract_order(Order)
	after ?ORDER_EXECUTION_DEADLINE -> 
		Manager ! timeout,
		retract_order(Order)
	end.

order_bank() ->
	order_bank([]).
order_bank(L) ->
	%Burde legge inn noe for å ta høyde for nettverksbrudd og fletting.
	%Se tanke
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

%Helpers
order_list_filter(Last_floor,Direction,L) ->
	%trenger mer testing om PossibleOrders er den den sier den er.
	PossibleOrders = lists:filter(fun(Elem) -> order_filter(Elem,Last_floor,Direction) end, L),
	if
		Direction =:= up ->
			lists:nth(1,PossibleOrders);
		Direction =:= down ->
			lists:last(PossibleOrders);
		true ->
			% ???
			lists:nth(1,PossibleOrders)
	end.

order_filter(#order{floor=Floor,direction=Dir},Last_floor, Direction) ->
	%{_,Floor,Dir} = Tup, 
	if 	((Dir =:= up) or (Dir =:= idle)) and (Direction =:= up) ->
			if
				Last_floor > Floor -> 
					false;
				Last_floor =< Floor -> 
					true
			end;
		((Dir =:= down) or (Dir =:= idle)) and (Direction =:= down) ->
			if
				Last_floor < Floor -> 
					false;
				Last_floor >= Floor-> 
					true
			end;
		Direction =:= idle -> 
			true;
		%Noe vi ikke har tatt høyde for?
		true -> false
	end.

remove_duplicates(L) ->
	%sorterer etter etasje, tilfeldigvis!
	%deretter etter down, idle, up
	%og ingen duplikater!
	ordsets:from_list(L).

send_to_queue_on_nodes(Msg) ->
	lists:foreach(fun(Node) -> {queue, Node} ! Msg end,nodes()).

get_orders() ->
	?QUEUE_PID ! {get_orders, self()},
	receive
		L -> NewL = L
	end,
	NewL.