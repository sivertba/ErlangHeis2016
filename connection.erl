-module (connection).
-compile(export_all).
-include("records_and_macros.hrl").

init() ->
	os:cmd("epmd -daemon"),

	{_ok, [LongIPtuple | _Tail]} = inet:getif(),
	NodeName = list_to_atom("heis@"++format_IP(element(1, LongIPtuple))),

	net_kernel:start([NodeName, longnames, 500]),
	erlang:set_cookie(node(), 'kake'),

	Hosts = net_adm:host_file(),
	connect(Hosts).

connect(Hosts) ->
	% request Erlang nodes from all hosts
	PidList = lists:map(fun(Elem) -> spawn_monitor(?MODULE, getNodes, [Elem, self()]) end, Hosts),
	
	Nodes = nodelist_builder([], PidList),
	lists:foreach(fun(Node) -> net_adm:ping(Node) end, Nodes),
	
	connect(Hosts).

getNodes(Host, Listener) ->
	timer:exit_after(2000, time_exceeded),
	Message = net_adm:names(Host),
	Listener ! {new_list_item, self(), Host, Message}.

% Collects replies from hosts with Erlang nodes, returns list of node names
% Runs until all hosts have replied or request timed out
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

% format IP from a tuple of numbers to string separated with "."
format_IP(IPtuple) ->
	[_Head | IPlist] = lists:flatmap(fun(X) -> ['.', X] end, tuple_to_list(IPtuple)),
	lists:concat(IPlist).
