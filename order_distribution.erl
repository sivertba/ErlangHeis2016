% i navaerende tilstand ikke en god modul
% og forovrig full av syntaksfeil

-module(order_distribution).
-include("orders.hrl").
-compile(export_all). %not really????

% state supervisor returnerer {{State, LastFloor, LastDir}, node()}
% State = idle/moving/stuck


cost_function({ElevState, ElevFloor, ElevLastDir}, OrderFloor) ->
  Difference = OrderFloor - ElevFloor, %if negative, order is below
  erlang:abs(Difference) + movement_penalty(ElevLastDir, Difference) + turn_penalty(ElevFloor, ElevLastDir, Difference).
  
% hjelpefunksjon for kostfunksjon (burde nøstes som lambda men det ble knot)
movement_penalty(idle, _) -> 0;
movement_penalty(_, 0) -> 1.5; %vi er pa vei vekk
movement_penalty(Dir, FloorDif) when (Dir == up andalso FloorDif > 0) orelse (Dir == down andalso FloorDif < 0) -> -0.5;
movement_penalty(Dir, FloorDif) when (Dir == up andalso FloorDif < 0) orelse (Dir == down andalso FloorDif > 0) -> 1.5.

turn_penalty(_ElevFloor, _Dir, 0) -> 0;
turn_penalty(ElevFloor, _Dir, _Dif) when ElevFloor == 0; ElevFloor == 3 -> 0;
turn_penalty(_ElevFloor, Dir, Dif) when (Dir == up andalso Dif > 0) orelse (Dir == down andalso Dif < 0) -> 0;
turn_penalty(_ElevFloor, Dir, Dif) when (Dir == up andalso Dif < 0) orelse (Dir == down andalso Dif > 0) -> 0.75.

% forklaring av verdier:
% om heisen er på vei mot ordren, er det bedre enn om den stod stille der den nettopp stod, men verre enn om den stod stille der den snart er
% om heisen er på vei vekk, er det +1 fordi den vil være en etasje lenger vekk når den kan handtere ordren, +0.5 fordi det tar tid til den er klar

% manager-oppførsel:
direction_picker() ->
  Elevators = elevator:get_states(),
  % hent ordreliste, med eldste først
  direction_picker(Elevators, Orders);
direction_picker(_Elevators, []) ->
  timer:sleep(200),
  direction_picker();
direction_picker(Elevators, Orders) ->
  [Oldest|Rest] = Orders,
  {BestCost, BestNode} = min(lists:map(fun({{State, Floor, Dir}, Node}) -> {cost_function({State, Floor, Dir} Oldest#order.floor), Node} end, Elevators))
  case BestNode == node() of
    true ->
      case BestCost of
          0 -> open_doors;
          _ ->
            if 
            % OBS! SelfFloor er udefinert RN
            Oldest#order.floor < SelfFloor -> down;
            Oldest#order.floor > SelfFloor -> up
          end
      end;
    false ->
      direction_picker({{SelfState, SelfFloor, SelfDir}, SelfNode}, NeighbStates, Rest)
  end
end.
  
  % prinsipp: se eldste ordre, er jeg narmeste heis? ja -> kjor, nei -> se etter nyere ordre
  % hvis ingen ordre, vent, prov igjen
  % problemer: to heiser som er like narme vil begge kjore
  % ved faerre enn 3 heiser resulterer det i ineffektiv oppforsel (i hvert fall ved 1 heis!)
  % NEI! :DD pga blindekjoringsprinsippet :D hvis det er noe pa veien stopper vi der forst