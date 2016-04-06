-module (connection).
-compile(export_all).

init() ->
	{_ok, [LongIPtuple | _Tail]} = inet:getif(),
	NodeName = list_to_atom("heis@"++tuple_to_stringIP(element(1, LongIPtuple))),
	net_kernel:start([NodeName, longnames, 500]),
	erlang:set_cookie(node(), 'kake'),


	Hosts = net_adm:host_file(),
	SelfPid = self(), % shit losning, unnskyld Kjetil & Erlang-guder

	connect(Hosts, SelfPid).
	
	%PingResults = lists:map(fun connection:unpackPing/1, NodeList),

connect(Hosts, SelfPid) ->
	PidList = lists:map(fun(Elem) -> spawn_monitor(?MODULE, getNodes, [Elem, SelfPid]) end, Hosts),
	% spawn_monitor returns {Pid, Ref}
	Nodes = nodelist_builder([], PidList),
	lists:foreach(fun(Node) -> net_adm:ping(Node) end, Nodes),
	connect(Hosts, SelfPid).


getNodes(Host, Listener) ->
	timer:exit_after(2000, time_exceeded),
	Message = net_adm:names(Host),
	Listener ! {new_list_item, self(), Host, Message}. %knotete

% unodvendig naa
%unpackPing({Name, _Port}) ->
	%net_adm:ping(list_to_atom(Name)).


nodelist_builder(NodeList, []) ->
	NodeList;
nodelist_builder(NodeList, PidList) ->
	receive
		{new_list_item, Pid, Host, {ok, RawNodes}} ->
			Nodes = lists:map(fun({Name, _Port}) -> list_to_atom(Name++"@"++atom_to_list(Host)) end, RawNodes),
			nodelist_builder(Nodes++NodeList, lists:keydelete(Pid, 1, PidList));
		{new_list_item, Pid, _Host, {error, _Reason}} ->
			nodelist_builder(NodeList, lists:keydelete(Pid, 1, PidList));
		{'DOWN', _Ref, process, Pid, _Reason} ->
			nodelist_builder(NodeList, lists:keydelete(Pid, 1, PidList))
	end.


tuple_to_stringIP(IPtuple) ->
	[_Head | IPlist] = lists:flatmap(fun(X) -> ['.', X] end, tuple_to_list(IPtuple)),
	lists:concat(IPlist).



%lists:foldl(fun(Int, String) -> String++'.'integer_to_list(Integer)++,"", tuple_to_list(IPtuple))