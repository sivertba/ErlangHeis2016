-module (order_handler).
-compile(export_all).

%Forslag til API:
-export ([	start/0,
			add_order/2,
			get_orders/0,
			get_next_order/3]).

-define(QUEUE_PID, queue).
-define(DETS_TABLE_NAME, "ordersETS").
-define(ORDER_EXECUTION_DEADLINE, 10000). %10 seconds

-record (order, {floor,direction,timestamp = erlang:timestamp()}).

%Må endres
start() ->
	%Burde gjøres i "main"
	register(?QUEUE_PID,spawn(?MODULE,order_bank,[])),
	
	dets:open_file(?DETS_TABLE_NAME, {filename, ?DETS_TABLE_NAME,acess,read}),
	% ???

	ok.

add_order(Floor,Direction) ->
	add_order(#order{floor = Floor, direction = Direction}).
add_order(Order) when Order#order.direction == command -> 
	?QUEUE_PID ! {add, Order};
add_order(Order) ->
	?QUEUE_PID ! {add, Order},
	send_to_queue_on_nodes({add, Order}).

get_orders() ->
	?QUEUE_PID ! {get_orders, self()},
	receive
		L -> L
	end.

%Denne er helt feil nå
get_next_order(Last_floor, Direction,Manager) ->
	L = get_orders(),
	if	L == [] -> 
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

order_monitor(Order,Manager) ->
	receive
		{order_handeld} -> 
			remove_order(Order);
		{order_aborted} -> 
			add_order(Order)
	after ?ORDER_EXECUTION_DEADLINE -> 
		Manager ! timeout,
		add_order(Order)
	end.

%Burde skrives om til å lagre på disk (dets) også
order_bank(L) ->
	%DETS HER
	receive
		{get_orders, Pid} ->
			Pid ! L,
			?MODULE:order_bank(L);
		{add, Order} -> 
			?MODULE:order_bank(remove_duplicates(Order,L));
		{remove, Order} -> 
			?MODULE:order_bank(lists:delete(Order, L))
	end.

remove_duplicates(Order,L) ->
	Bool = lists:any(fun(E) -> order_match(E,Order) end, L),
	if
		 Bool->
			L;
		true -> 
			L ++ Order	
	end.	

order_match(A,B) -> 
	if
		(A#order.floor == B#order.floor) andalso (A#order.direction == B#order.direction) ->
			true;
		(A#order.floor /= B#order.floor) or (A#order.direction /= B#order.direction) ->
			false
	end.

floor_match(A,B) -> 
	if
		(A#order.floor /= B#order.floor) -> true;
		(A#order.floor /= B#order.floor) ->	false
	end.

timestamp_compare(A,B) -> 
	if 
		A#order.timestamp =< B#order.timestamp -> true;
		A#order.timestamp > B#order.timestamp -> false
	end.

floor_compare(A,B) ->
	if
		A#order.floor =< B#order.floor -> true;
		A#order.floor > B#order.floor -> false
	end.

order_is_in_list(O,List) ->
	lists:any(fun(E) -> order_match(E,O) end ,List).

order_list_filter(Last_floor,Direction,L) ->
	PossibleOrders = lists:filter(fun(Elem) -> on_path(Elem,Last_floor,Direction) end, L),
	SortedOrders = lists:sort(fun(A,B) -> timestamp_compare(A,B) end, PossibleOrders),
	lists:nth(1,SortedOrders).

on_path(#order{floor=Floor,direction=Dir},Last_floor, Direction) ->
	if 	((Dir == up) or (Dir == command)) and (Direction == up) ->
			if
				Last_floor > Floor -> 
					false;
				Last_floor =< Floor -> 
					true
			end;
		((Dir == down) or (Dir == command)) and (Direction == down) ->
			if
				Last_floor < Floor -> 
					false;
				Last_floor >= Floor-> 
					true
			end;
		Direction == command -> 
			true;
		true -> 
			false
	end.

send_to_queue_on_nodes(Msg) ->
	lists:foreach(fun(Node) -> {?QUEUE_PID, Node} ! Msg end,nodes()).

%Må tenke bedre ...
orderUpdate(L) ->
	send_to_queue_on_nodes({get_orders,self()}),
	receive
		Orders -> ordsets:intersection(L,Orders)
			lists:append(L,Orders)
		after 50 -> ordsets:to_listj(L)
	end.