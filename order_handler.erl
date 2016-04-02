-module (order_handler).
-compile(export_all).

%Forslag til API:
-export ([	start/0,
			add_order/2,
			get_orders/0,
			remove_order/1]).

-define(QUEUE_PID, queue).
-define(DETS_TABLE_NAME, "ordersETS").
-define(ORDER_EXECUTION_DEADLINE, 10000). %10 seconds

-record (order, {floor,direction,timestamp = erlang:timestamp()}).

%Må endres
start() ->
	
	%Update from storage
	dets:open_file(?DETS_TABLE_NAME, [{type,bag}]),
    Orders_From_Disk = dets:lookup(?DETS_TABLE_NAME, order),
    dets:close(?DETS_TABLE_NAME),
	
	%Burde gjøres i "main"
	register(?QUEUE_PID,spawn(?MODULE,order_bank,[Orders_From_Disk])),
	
    %Request orders from other nodes and add
    lists:foreach(fun(E) -> add_order(E) end, get_orders_from_connected_nodes()),
	ok.

add_order(Floor,Direction) ->
	add_order(#order{floor = Floor, direction = Direction}).
	
add_order(Order) when Order#order.direction == command -> 
	?QUEUE_PID ! {add, Order};
add_order(Order) ->
	?QUEUE_PID ! {add, Order},
	send_to_queue_on_nodes({add, Order}).

remove_order(Order) -> 
	?QUEUE_PID ! {remove, Order},
	send_to_queue_on_nodes({remove, Order}).

get_orders() ->
	?QUEUE_PID ! {get_orders, self()},
	receive
		L -> L
	end.

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

order_bank(L) ->
	dets:open_file(?DETS_TABLE_NAME, [{type,bag}]),
	dets:delete_all_objects(?DETS_TABLE_NAME),
	lists:foreach(fun(E) -> dets:insert(?DETS_TABLE_NAME, E) end, L),
	dets:close(?DETS_TABLE_NAME),

	receive
		{get_orders, Pid} ->
			Pid ! L,
			?MODULE:order_bank(L);
		{add, Order} ->
			Duplicates = is_duplicates(Order,L),
			if
				Duplicates ->
					?MODULE:order_bank(L);
				not Duplicates ->
					?MODULE:order_bank(L++[Order])
			end;
		{remove, Order} -> 
			?MODULE:order_bank(lists:delete(Order, L))
	end.

is_duplicates(Order,List) ->
	lists:any(fun(E) -> order_match(E,Order) end ,List).

order_match(A,B) -> 
	if
		(A#order.floor == B#order.floor) andalso (A#order.direction == B#order.direction) ->
			true;
		(A#order.floor /= B#order.floor) or (A#order.direction /= B#order.direction) ->
			false
	end.

floor_match(A,B) -> 
	if
		(A#order.floor == B#order.floor) -> true;
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

is_command_order(Order) when Order#order.direction == command -> true;
is_command_order(Order) when Order#order.direction /= command -> false.

% nar bruker vi denne? nar trenger vi eldste ordre??
orders_on_path(Last_floor,Direction,L) ->
	PossibleOrders = lists:filter(fun(Elem) -> on_path(Elem,Last_floor,Direction) end, L),
	[First | _Rest] = lists:sort(fun(A,B) -> timestamp_compare(A,B) end, PossibleOrders),
	First.

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

get_orders_from_connected_nodes() ->
	Receiver = spawn(?MODULE,merge_received,[[], self()]),
	send_to_queue_on_nodes({get_orders, Receiver}),
	receive
		List -> 
			List
	after 100 -> 
			[]
	end.

merge_received(List,Pid) -> 
	receive
		L -> 
			RetList = List ++ (lists:filter(fun(E) -> not is_command_order(E) end, L)),
			merge_received(RetList,Pid)
	after 50 ->
		Pid ! List
	end.

send_to_queue_on_nodes(Msg) ->
	lists:foreach(fun(Node) -> {?QUEUE_PID, Node} ! Msg end,nodes()).
