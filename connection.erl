-module (connection).
-compile(export_all).
-include("records_and_macros.hrl").

init() ->
	{_ok, [LongIPtuple | _Tail]} = inet:getif(),
	NodeName = list_to_atom("heis@"++format_IP(element(1, LongIPtuple))),
	case net_kernel:start([NodeName, longnames, 500]) of
		{error, Reason} -> erlang:display(Reason);
		_ -> ok
	end,
	erlang:set_cookie(node(), 'kake'),

	Hosts = net_adm:host_file(),
	erlang:display(Hosts),
	connect(Hosts).

% HVIS DEN IKKE KJORER, FIKS LINJE 56 SOM BESKREVET DER
connect(Hosts) ->
	PidList = lists:map(fun(Elem) -> spawn_monitor(?MODULE, getNodes, [Elem, self()]) end, Hosts),
	
	% det som skal ha funket onsdag:
	Nodes = nodelist_builder([], PidList),
	lists:foreach(fun(Node) -> net_adm:ping(Node) end, Nodes),
	
	%funket torsdag, ikke lordag:
	% problem: world_list henger hvis den kjores pa tom liste
	%ResponsiveHosts = hostlist_builder([], PidList),
	%net_adm:world_list(ResponsiveHosts),
	
	% burde funke, men gjor ikke det:
	%lists:foreach(fun(Host) -> net_adm:ping(list_to_atom("heis@"++atom_to_list(Host))) end, ResponsiveHosts),
	
	connect(Hosts).

getNodes(Host, Listener) ->
	timer:exit_after(2000, time_exceeded),
	Message = net_adm:names(Host),
	Listener ! {new_list_item, self(), Host, Message}.

hostlist_builder(HostList, []) ->
	HostList;
hostlist_builder(HostList, PidList) ->
	receive
		{new_list_item, Pid, Host, {ok, _Nodes}} ->
			hostlist_builder([Host | HostList], lists:keydelete(Pid, 1, PidList));
		{new_list_item, Pid, _Host, {error, _Reason}} ->
			hostlist_builder(HostList, lists:keydelete(Pid, 1, PidList));
		{'DOWN', _Ref, process, Pid, _Reason} ->
			hostlist_builder(HostList, lists:keydelete(Pid, 1, PidList))
	end.

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

format_IP(IPtuple) ->
	[_Head | IPlist] = lists:flatmap(fun(X) -> ['.', X] end, tuple_to_list(IPtuple)),
	lists:concat(IPlist).
