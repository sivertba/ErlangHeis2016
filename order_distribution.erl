-module(order_distribution).
-include("records_and_macros.hrl").
-compile(export_all).

cost_function({ElevState, ElevFloor, ElevLastDir}, OrderFloor, OrderDir) ->
  case ElevState of
  	stuck -> 3*?NUMBER_OF_FLOORS;
  	_ ->
      Difference = OrderFloor - ElevFloor, %if negative, order is below
      erlang:abs(Difference) + movement_penalty(ElevState, ElevLastDir, Difference) + turn_penalty(ElevState, ElevFloor, ElevLastDir, OrderFloor) + order_dir_penalty(ElevLastDir, OrderFloor, OrderDir)
  end.
  
% Cost modifier if traveling to or from order floor
movement_penalty(stationary, _Dir, _Dif) -> 0;
movement_penalty(_State, _Dir, 0) -> 1.5;
movement_penalty(_State, Dir, FloorDif) when (Dir == up andalso FloorDif > 0) orelse (Dir == down andalso FloorDif < 0) -> -0.5;
movement_penalty(_State, Dir, FloorDif) when (Dir == up andalso FloorDif < 0) orelse (Dir == down andalso FloorDif > 0) -> 1.5.

% Penalty for changing direction of travel
turn_penalty(stationary, ElevFloor, _ElevDir, _OrderFloor) when ElevFloor == 0 orelse ElevFloor == (?NUMBER_OF_FLOORS-1) -> 0;
turn_penalty(moving, ElevFloor, ElevDir, _OrderFloor) when (ElevFloor == 1 andalso ElevDir == down); (ElevFloor == (?NUMBER_OF_FLOORS-2) andalso ElevDir == up) -> 0;
turn_penalty(_ElevState, ElevFloor, ElevDir, OrderFloor) when (ElevDir == up andalso OrderFloor < ElevFloor) orelse (ElevDir == down andalso OrderFloor > ElevFloor) -> 0.75;
turn_penalty(_ElevState, _ElevFloor, _ElevDir, _OrderFloor) -> 0.

% Elevators are not allowed to handle orders in wrong direction if there are orders beyond, cost must agree
order_dir_penalty(_ElevDir, OrderFloor, _OrderDir) when OrderFloor == 0 orelse OrderFloor == ?NUMBER_OF_FLOORS-1 -> 0;
order_dir_penalty(ElevDir, _OrderFloor, OrderDir) when OrderDir /= command andalso ElevDir /= OrderDir -> ?NUMBER_OF_FLOORS-2+0.25;
order_dir_penalty(_ElevDir, _OrderFloor, _OrderDir) -> 0.

% Elevators expects a list of tuples {{State, LastFloor, LastDir}, node()}
action_select(_Elevators, []) -> wait;
action_select(Elevators, Orders) ->
  {value, {LocalState, _Node}, OtherElevs} = lists:keytake(node(), 2, Elevators),
  OurOrders = lists:keysort(1, lists:map(fun(Order) -> {cost_function(LocalState, Order#order.floor, Order#order.direction), Order} end, Orders)),
  OtherOrders = lists:keysort(1, lists:flatmap(fun({ElevState, Node}) -> lists:map(fun(Order) -> {cost_function(ElevState, Order#order.floor, Order#order.direction), Order, Node} end, lists:filter(fun(Order) -> Order#order.direction /= command end, Orders)) end, OtherElevs)),
  NextOrder = find_next_order(OurOrders, OtherOrders),
  case NextOrder of
    none -> wait;
    Order ->
      {_State, ElevFloor, _Dir} = LocalState,
      if
        Order#order.floor == ElevFloor -> open_doors; 
        Order#order.floor < ElevFloor -> down;
        Order#order.floor > ElevFloor -> up
      end
  end.

find_next_order([], _OtherOrders) -> none;
find_next_order([{BestCost, BestOrder}| Rest], OtherOrders) ->
  case lists:keyfind(BestOrder, 2, OtherOrders) of
    false ->
      BestOrder;
    {OtherCost, _Order, Node} when (OtherCost > BestCost) orelse (OtherCost == BestCost andalso Node > node()) ->
      BestOrder;
    _ ->
      find_next_order(Rest, OtherOrders)
  end.
