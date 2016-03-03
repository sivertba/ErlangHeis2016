-module (test).
-compile(export_all).

init() ->
	
	Hosts = net_adm:host_file(),
	Mum = self(),

	lists:foreach(fun(Elem) -> spawn(?MODULE,getNodes,[Elem,Mum]) end, Hosts),
	
	NodeListPID = spawn_monitor(?MODULE,list_control,[[]]),


	 = list_builder([], NodeListPID).


getNodes(Host, Listener) ->
	timer:exit_after(3000, time_exceeded),
	Message = net_adm:names(Host),
	Listener ! {new_list_item, self(), Message}.


tup(Tupel) ->
	{Name,_Port} = Tupel,
	net_adm:ping(Name).


list_control(List) ->
	receive
		{add, ToAdd} ->
			list_control(List ++ ToAdd);
		{send, Pid} ->
			Pid ! {List}
	end.


list_builder(NodeList, []) ->
	NodeList;
list_builder(NodeList, PidList) ->
	receive
		{new_list_item, Pid, {ok, Item}} ->
			list_builder([Item|List], lists:delete(Pid, PidList));
		{new_list_item, Pid, {error, _Reason}} ->
			list_builder(List, lists:delete(Pid, PidList));
		{'DOWN', _Ref, process, PID, _Reason} ->
			list_builder(List, lists:delete(Pid, PidList))
	end.
	