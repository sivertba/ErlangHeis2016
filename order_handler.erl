-module (order_handler).
-compile(export_all).

-define(ORDER_EXECUTION_TIME, 10000).

%Tenker her at vi bruker noe sånt som:
%	register(OrderBankPID,Pid)
%Ett eller annet sted

get_orders(Pid, OrderBankPID) -> OrderBankPID ! {get_orders, Pid}.
add_order(Order, OrderBankPID) -> OrderBankPID ! {add, Order}.
remove_order(Order, OrderBankPID) -> OrderBankPID ! {remove, Order}.
retract_order(Order, OrderBankPID) -> OrderBankPID ! {retract, Order}.

%checks if order is executed within some time.
order_guard(Order) ->
	receive
		{order_handeld} -> 
			remove_order(Order)
	after ?ORDER_EXECUTION_TIME -> 
		retract_order(Order)
	end.

order_bank() ->
	?MODULE:order_bank([]).
order_bank(L) ->
	receive
		{get_orders, Pid} ->
			Pid ! L,
			?MODULE:order_bank(L);
		{add, Order} -> 
			NewL = lists:append(L,[Order]),
			?MODULE:order_bank(?MODULE:remove_duplicates(NewL));
		{remove, Order} -> 
			?MODULE:order_bank(lists:delete(Order, L));
		{retract, Order} ->
			NewL = Order ++ L,
			?MODULE:order_bank(?MODULE:remove_duplicates(NewL))
	end.

%Helpers
remove_duplicates(L) ->
	TempSet = sets:from_list(L),
	sets:to_list(TempSet).