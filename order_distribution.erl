% i navaerende tilstand ikke en god modul
% og forovrig full av syntaksfeil

-module(order_distribution).
-include("records_and_macros.hrl").
-compile(export_all). %not really????

% state supervisor returnerer {{State, LastFloor, LastDir}, node()}
% State = stationary/moving/stuck


cost_function({ElevState, ElevFloor, ElevLastDir}, OrderFloor) ->
  Difference = OrderFloor - ElevFloor, %if negative, order is below
  case ElevState of
  	stuck -> 5;
  	_ ->
  		Difference = OrderFloor - ElevFloor,
  		erlang:abs(Difference) + movement_penalty(ElevState, ElevLastDir, Difference) + turn_penalty(ElevState, ElevFloor, ElevLastDir, Difference)
  end.
  
% hjelpefunksjon for kostfunksjon (burde nøstes som lambda men det ble knot)
movement_penalty(stationary, _Dir, _Dif) -> 0;
movement_penalty(_State, _Dir, 0) -> 1.5; %vi er pa vei vekk
movement_penalty(_State, Dir, FloorDif) when (Dir == up andalso FloorDif > 0) orelse (Dir == down andalso FloorDif < 0) -> -0.5;
movement_penalty(_State, Dir, FloorDif) when (Dir == up andalso FloorDif < 0) orelse (Dir == down andalso FloorDif > 0) -> 1.5.

turn_penalty(stationary, ElevFloor, _Dir, _Dif) when ElevFloor == 0; ElevFloor == (?NUMBER_OF_FLOORS-1) -> 0;
turn_penalty(moving, ElevFloor, Dir, _Dif) when (ElevFloor == 1 andalso Dir == down); (ElevFloor == (?NUMBER_OF_FLOORS-2) andalso Dir == up) -> 0;
turn_penalty(stationary, _ElevFloor, Dir, Dif) when (Dir == up andalso Dif >= 0) orelse (Dir == down andalso Dif =< 0) -> 0;
turn_penalty(moving, _ElevFloor, Dir, Dif) when (Dir == up andalso Dif > 0) orelse (Dir == down andalso Dif < 0) -> 0;
turn_penalty(_ElevState, _ElevFloor, _Dir, _Dif) -> 0.75.

% forklaring av verdier:
% om heisen er på vei mot ordren, er det bedre enn om den stod stille der den nettopp stod, men verre enn om den stod stille der den snart er
% om heisen er på vei vekk, er det +1 fordi den vil være en etasje lenger vekk når den kan handtere ordren, +0.5 fordi det tar tid til den er klar

% manager-oppførsel:
action_select(_Elevators, []) -> wait;
action_select(Elevators, Orders) ->
  [Oldest | Rest] = Orders,
  {_BestCost, BestNode, BestFloor} = lists:min(lists:map(fun({{State, Floor, Dir}, Node}) -> {cost_function({State, Floor, Dir}, Oldest#order.floor), Node, Floor} end, Elevators)),
  case BestNode == node() of
    true ->
    	if
    		Oldest#order.floor == BestFloor -> doors_open; 
    		Oldest#order.floor < BestFloor -> down;
    		Oldest#order.floor > BestFloor -> up
    	end;
    false ->
      action_select(Elevators, Rest)
  end.
  
  % prinsipp: se eldste ordre, er jeg narmeste heis? ja -> kjor, nei -> se etter nyere ordre
  % hvis ingen ordre, vent, prov igjen
  % problemer: to heiser som er like narme vil begge kjore
  % ved faerre enn 3 heiser resulterer det i ineffektiv oppforsel (i hvert fall ved 1 heis!)
  % NEI! :DD pga blindekjoringsprinsippet :D hvis det er noe pa veien stopper vi der forst