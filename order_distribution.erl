-module(order_distribution).
-include("records_and_macros.hrl").
-compile(export_all). %not really????

% state supervisor returnerer {{State, LastFloor, LastDir}, node()}
% State = stationary/moving/stuck


cost_function({ElevState, ElevFloor, ElevLastDir}, OrderFloor, OrderDir) ->
  case ElevState of
  	stuck -> 10;
  	_ ->
      Difference = OrderFloor - ElevFloor, %if negative, order is below
      Cost = erlang:abs(Difference) + movement_penalty(ElevState, ElevLastDir, Difference) + turn_penalty(ElevState, ElevFloor, ElevLastDir, OrderFloor, OrderDir),
      erlang:display(["Order ", OrderFloor, OrderDir, "Costs ", Cost, "From ", ElevFloor, ElevLastDir]),
      Cost
  end.
  
% hjelpefunksjon for kostfunksjon (burde nøstes som lambda men det ble knot)
movement_penalty(stationary, _Dir, _Dif) -> 0;
movement_penalty(_State, _Dir, 0) -> 1.5; %vi er pa vei vekk
movement_penalty(_State, Dir, FloorDif) when (Dir == up andalso FloorDif > 0) orelse (Dir == down andalso FloorDif < 0) -> -0.5;
movement_penalty(_State, Dir, FloorDif) when (Dir == up andalso FloorDif < 0) orelse (Dir == down andalso FloorDif > 0) -> 1.5.

%Penalty for getting there:
turn_penalty(stationary, ElevFloor, _ElevDir, _OrderFloor, _OrderDir) when ElevFloor == 0 orelse ElevFloor == (?NUMBER_OF_FLOORS-1) -> 0;
turn_penalty(moving, ElevFloor, ElevDir, _OrderFloor, _OrderDir) when (ElevFloor == 1 andalso ElevDir == down); (ElevFloor == (?NUMBER_OF_FLOORS-2) andalso ElevDir == up) -> 0;
turn_penalty(_ElevState, ElevFloor, ElevDir, OrderFloor, _OrderDir) when (ElevDir == up andalso OrderFloor < ElevFloor) orelse (ElevDir == down andalso OrderFloor > ElevFloor) -> 2;
% Penalty for aiming at orders in wrong direction:
turn_penalty(_ElevState, _ElevFloor, _ElevDir, OrderFloor, _OrderDir) when OrderFloor == 0 orelse OrderFloor == (?NUMBER_OF_FLOORS-1) -> 0;
turn_penalty(_ElevState, _ElevFloor, ElevDir, _OrderFloor, OrderDir) when OrderDir /= command andalso ElevDir /= OrderDir-> 3;
%turn_penalty(stationary, _ElevFloor, Dir, Dif) when (Dir == up andalso Dif > 0) orelse (Dir == down andalso Dif < 0) -> 0;
%turn_penalty(moving, _ElevFloor, Dir, Dif) when (Dir == up andalso Dif > 0) orelse (Dir == down andalso Dif < 0) -> 0;
turn_penalty(_ElevState, _ElevFloor, _ElevDir, _OrderFloor, _OrderDir) -> 0.

% forklaring av verdier:
% om heisen er på vei mot ordren, er det bedre enn om den stod stille der den nettopp stod, men verre enn om den stod stille der den snart er
% om heisen er på vei vekk, er det +1 fordi den vil være en etasje lenger vekk når den kan handtere ordren, +0.5 fordi det tar tid til den er klar

% manager-oppførsel:
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

  %[Oldest | Rest] = Orders,
  %{_BestCost, BestNode, BestFloor} 
  %{_BestCost, BestNode, BestFloor} = lists:min(lists:map(fun({{State, Floor, Dir}, Node}) -> {cost_function({State, Floor, Dir}, Oldest#order.floor, Oldest#order.direction), Node, Floor} end, Elevators)),
  %case BestNode == node() of
  %  true ->
  %  	if
  %  		Oldest#order.floor == BestFloor -> open_doors; 
  %  		Oldest#order.floor < BestFloor -> down;
  %  		Oldest#order.floor > BestFloor -> up
  %  	end;
  %  false ->
  %  	action_select(Elevators, Rest)
  %end.
  
  % prinsipp: se eldste ordre, er jeg narmeste heis? ja -> kjor, nei -> se etter nyere ordre
  % hvis ingen ordre, vent, prov igjen
  % problemer: to heiser som er like narme vil begge kjore
  % ved faerre enn 3 heiser resulterer det i ineffektiv oppforsel (i hvert fall ved 1 heis!)
  % NEI! :DD pga blindekjoringsprinsippet :D hvis det er noe pa veien stopper vi der forst


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
