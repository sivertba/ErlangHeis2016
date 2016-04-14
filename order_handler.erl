-module (order_handler).
-compile(export_all).
-include("records_and_macros.hrl").

-export ([	start/0,
			is_order/2,
			add_order/2,
			get_orders/0,
			remove_order/1]).


start() ->
	
	%Update from storage
	dets:open_file(?DETS_TABLE_NAME, [{type,bag}]),
    Orders_From_Disk = dets:lookup(?DETS_TABLE_NAME, order),
    dets:close(?DETS_TABLE_NAME),
	
	register(?QUEUE_PID,spawn(?MODULE,order_bank,[Orders_From_Disk])),
	
    %Request orders from other nodes andalso add
    lists:foreach(fun(E) -> add_order(E) end, get_orders_from_connected_nodes()),

    %Handels network errors
    spawn(?MODULE,start_monitoring_of_nodes,[]),

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
	dets:open_file(?DETS_TABLE_NAME, [{type,bag}]),
    Orders_From_Disk = dets:lookup(?DETS_TABLE_NAME, order),
    dets:close(?DETS_TABLE_NAME),
    Orders_From_Disk.

order_bank(L) ->
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
					dets:open_file(?DETS_TABLE_NAME, [{type,bag}]),
    				dets:insert(?DETS_TABLE_NAME, Order),
    				dets:close(?DETS_TABLE_NAME),
					?MODULE:order_bank(L++[Order])
			end;
		{remove, Order} ->
			dets:open_file(?DETS_TABLE_NAME, [{type,bag}]),
    		dets:delete_object(?DETS_TABLE_NAME, Order),
    		dets:close(?DETS_TABLE_NAME),
			?MODULE:order_bank(lists:delete(Order, L))
	end.

to_remove(Floor,Direction) -> 
	Case = orders_beyond(Floor, Direction),
	case Case of
		true -> 
			{lists:filter(fun(E) -> on_path(E,Floor,Direction) andalso floor_match(#order{floor=Floor,direction=Direction},E) end, get_orders()), Case};
		false ->
			{lists:filter(fun(E) -> floor_match(#order{floor=Floor,direction=Direction}, E) end, get_orders()), Case}
	end.

is_duplicates(Order,List) ->
	lists:any(fun(E) -> order_match(E,Order) end ,List).

order_match(A,B) when (A#order.floor == B#order.floor) andalso (A#order.direction == B#order.direction) -> true;
order_match(A,B) when (A#order.floor /= B#order.floor) orelse (A#order.direction /= B#order.direction) -> false.

floor_match(A,B) when (A#order.floor == B#order.floor) -> true;
floor_match(A,B) when (A#order.floor /= B#order.floor) -> false. 

floor_compare(A,B) when A#order.floor =< B#order.floor -> true;
floor_compare(A,B) when A#order.floor > B#order.floor -> false.

is_command_order(Order) when Order#order.direction == command -> true;
is_command_order(Order) when Order#order.direction /= command -> false.

is_order(Floor,Direction) ->
	is_duplicates(#order{floor = Floor, direction = Direction}, get_orders()).

on_path(#order{floor=OrderFloor,direction=OrderDir}, ElevFloor, ElevDir) ->
	case ElevDir of
		up  when (ElevFloor == ?NUMBER_OF_FLOORS-1) orelse (OrderDir /= down andalso ElevFloor =< OrderFloor) ->
			true;
		down when (ElevFloor == 0) orelse (OrderDir /= up andalso ElevFloor >= OrderFloor) ->
			true;
		_ ->
			false
	end.

orders_beyond(Floor, Dir) ->
	Orders = get_orders(),
	case Dir of
		up ->
			NextFloors = lists:seq(Floor+1, ?NUMBER_OF_FLOORS-1);
		down ->
			NextFloors = lists:seq(0, Floor-1)
	end,
	lists:any(fun(F) -> lists:any(fun(Order) -> floor_match(#order{floor=F}, Order) end, Orders) end, NextFloors).

get_orders_from_connected_nodes() ->
	Receiver = spawn(?MODULE,merge_received,[[], self(),0]),
	send_to_queue_on_nodes({get_orders, Receiver}),
	receive
		List -> 
			List
	after 100 -> 
			[]
	end.

%Makes a list of every connected elevator's orders and passes it on
merge_received(List,Pid, Counter) ->
	receive
		L -> 
			RetList = List ++ (lists:filter(fun(E) -> not is_command_order(E) end, L)),
			MaxNodesReached = Counter == length(nodes()),
			if
				MaxNodesReached ->
					Pid ! RetList;
				not MaxNodesReached ->
					?MODULE:merge_received(ordsets:from_list(RetList),Pid, Counter + 1)
			end
	after 50 ->
		Pid ! List
	end.

send_to_queue_on_nodes(Msg) ->
	lists:foreach(fun(Node) -> {?QUEUE_PID, Node} ! Msg end,nodes()).

start_monitoring_of_nodes() ->
	watcher({0,0,0}).

watcher({0,0,0}) ->
	global_group:monitor_nodes(true),
	watcher({0,0,1});
watcher(Timestamp) ->
	receive 
		{nodedown, _Node} ->
			watcher(Timestamp);

		{nodeup, _Node} ->			
			MyOrders = get_orders(),
			OtherOrders = get_orders_from_connected_nodes(),
			lists:foreach(fun(E) -> remove_order(E) end, MyOrders),
			lists:foreach(fun(E) -> add_order(E) end, ordsets:from_list(MyOrders ++ OtherOrders))

	after 30000 ->
		MyOrders = get_orders(),
		OtherOrders = get_orders_from_connected_nodes(),
		ToAdd = OtherOrders -- MyOrders,
		case ToAdd of
			[] ->
				watcher(Timestamp);
			_ -> 
				lists:foreach(fun(E) -> remove_order(E) end, MyOrders),
				lists:foreach(fun(E) -> add_order(E) end, ordsets:from_list(MyOrders ++ OtherOrders)),
				watcher(Timestamp)
		end
	end,
	?MODULE:watcher({0,0,1}).