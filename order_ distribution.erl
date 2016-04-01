% i navaerende tilstand ikke en god modul
% og forovrig full av syntaksfeil

-module(order_distribution).
-compile(export_all). %not really????

cost_function(ElevFloor, ElevState, OrderFloor) ->
  % ElevFloor er siste besøkte etasje, ElevState er up/down/idle (ikke helt samsvarende med faktiske states)
  Difference = OrderFloor - ElevFloor, %if negative, order is below
  erlang:abs(Difference)+movement_penalty(ElevState, Difference).
  
% hjelpefunksjon for kostfunksjon (burde nøstes som lambda men det ble knot)
movement_penalty(idle, _) -> 0;
movement_penalty(_, 0) -> 1.5; %vi er pa vei vekk
movement_penalty(State, FloorDif) when (State == up and FloorDif > 0) or (State == down and FloorDif < 0) -> -0.5;
movement_penalty(State, FloorDif) when (State == up and FloorDif < 0) or (State == down and FloorDif > 0) -> 1.5.
% burde bruke andalso/orelse for kortslutting?

% forklaring av verdier:
% om heisen er på vei mot ordren, er det bedre enn om den stod stille der den nettopp stod, men verre enn om den stod stille der den snart er
% om heisen er på vei vekk, er det +1 fordi den vil være en etasje lenger vekk når den kan handtere ordren, +0.5 fordi det tar tid til den er klar

% manager-oppførsel:
direction_picker() ->
  % hent egen state
  % hent inn  naboenes states
  % hent ordreliste, med eldste først
  direction_picker({SelfFloor, SelfState}, NeighbStates, Orders);
direction_picker({SelfFloor, SelfState}, NeighbStates, []) ->
  % vent litt
  direction_picker();
direction_picker({SelfFloor, SelfState}, NeighbStates, Orders) ->
  [Oldest|Rest] = Orders,
  SelfCost = cost_function(SelfFloor, SelfState, Oldest.Floor),
  NeighbCosts = lists:foreach(fun({Floor, State}) -> cost_function(Floor, State, Oldest.Floor), NeighbStates),
  case lists:any(fun(Cost) when Cost < SelfCost -> true; fun(Cost) when Cost >= SelfCost -> false end, NeighbCosts) of
    true -> direction_picker({SelfFloor, SelfState}, NeighbStates, Rest);
    false -> (Oldest.Floor - SelfFlooor)/abs(Oldest.Floor - SelfFlooor) %+/- 1, burde skrives om til if og atomer
  end.
  
  % prinsipp: se eldste ordre, er jeg narmeste heis? ja -> kjor, nei -> se etter nyere ordre
  % problemer: to heiser som er like narme vil begge kjore
  % ved faerre enn 3 heiser resulterer det i ineffektiv oppforsel (i hvert fall ved 1 heis!)
  % hvis ingen ordre, vent, prov igjen
  
  % burde hver heis i stedet se etter narmeste ordre? -> risikerer å sette fast ensom heis mellom 2 etg
