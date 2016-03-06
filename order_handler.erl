-module (order_handler).
-compile(export_all).

-define(ORDER_EXECUTION_TIME, 20000). %20 seconds
-define (ORDER_BANK_PID, orderbank).

-record (order, {floor,direction}).

%Tenker her at vi bruker noe sånt som:
%	register(?ORDER_BANK_PID,?ORDER_BANK_PID)
%Ett eller annet sted, sånn at vi kan kalle på tvers av noder.

%TANKE
%Vi lager to ordrebanker, en kø og en som har oversikt over 
%hvem som blir behandlet, og så tar vi det som ikke blir behandlet, 
%men som fortsatt er i køen og sender inn til get_next ...

start() ->
	register(orderbank,spawn(?MODULE,order_bank,[])).

make_order(Floor, Direction) ->
	#order{floor = Floor, direction = Direction}.

add_order(Order) when Order#order.direction =:= none -> 
	?ORDER_BANK_PID ! {add, Order};
add_order(Order) ->
	?ORDER_BANK_PID ! {add, Order},
	send_to_orderbank_on_nodes({add, Order}).

remove_order(Order) -> 
	?ORDER_BANK_PID ! {remove, Order},
	send_to_orderbank_on_nodes({remove, Order}).

retract_order(Order) when Order#order.direction =:= none -> 
	?ORDER_BANK_PID ! {retract, Order};
retract_order(Order) ->
	?ORDER_BANK_PID ! {retract, Order},
	send_to_orderbank_on_nodes({retract, Order}).

get_next_order(Last_floor, Direction) ->
	?ORDER_BANK_PID ! {get_orders, self()},
	receive
		L ->
			if
				L =:= [] -> 
					?MODULE:get_next_order(Last_floor, Direction);
		 		true -> 
		 			order_list_filter(Last_floor,Direction,L)
			end
	end.
	

%Denne burde ta hensyn til mer...
order_monitor(Order) ->
	receive
		{order_handeld} -> 
			remove_order(Order);
		{order_aborted} ->
			retract_order(Order)	
	after ?ORDER_EXECUTION_TIME -> 
		retract_order(Order)
	end.

order_bank() ->
	?MODULE:order_bank([]).
order_bank(L) ->
	%Burde legge inn noe for å ta høyde for nettverksbrudd og fletting.
	%Se tanke
	receive
		{get_orders, Pid} ->
			Pid ! L,
			?MODULE:order_bank(?MODULE:remove_duplicates(L));
		{add, Order} -> 
			NewL = lists:append(L,[Order]),
			?MODULE:order_bank(?MODULE:remove_duplicates(NewL));
		{remove, Order} -> 
			?MODULE:order_bank(lists:delete(Order, L));
		{retract, Order} ->
			NewL = lists:append([Order],L),
			?MODULE:order_bank(?MODULE:remove_duplicates(NewL))
	end.

%Helpers

order_list_filter(Last_floor,Direction,L) ->
	if  (Direction =:= up) or  (Direction =:= none) ->
			PossibleOrders = lists:filter(fun(Elem) -> order_filter(Elem,Last_floor,Direction) end, L),
			ok;
		(Direction =:= down) ->
			PossibleOrders = lists:filter(fun(Elem) -> order_filter(Elem,Last_floor,Direction) end, L),
			ok
	end.

order_filter(#order{floor=Floor,direction=Dir},Last_floor, Direction) ->
	%{_,Floor,OrderDir} = Tup, 
	if 	((Dir =:= up) or (Dir =:= none)) and (Direction =:= up) ->
			if
				Last_floor < Floor -> 
					false;
				Last_floor >= Floor -> 
					true
			end;
		((Dir =:= down) or (Dir =:= none)) and (Direction =:= down) ->
			if
				Last_floor < Floor -> 
					false;
				Last_floor >= Floor-> 
					true
			end
	end.

remove_duplicates(L) ->
	%gjør at det er en tilfeldig rekkefølge på ordre ...
	%men ingen duplikater!
	ordsets:from_list(L).

send_to_orderbank_on_nodes(Msg) ->
	lists:foreach(fun(Node) -> {orderbank, Node} ! Msg end,nodes()).

%TESTING
print_order(Order) ->
	io:format("Floor: ~w, Direction: ~w~n", [Order#order.floor, Order#order.direction]).

get_orders(Recipient) -> 
	?ORDER_BANK_PID ! {get_orders, Recipient}.

print_orders() ->
	?ORDER_BANK_PID ! {get_orders, self()},
	receive
		L -> NewL = L
	end,
	NewL.