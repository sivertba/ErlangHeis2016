-module (elevatorServer).
-behaviour (gen_server).

%Client APIs
-export ([	start_server/0,
			addOrder/2, 
			removeOrder/2, ]).

%	gen_server callbacks
-export ([	init/1, 
			terminate/2,
			handle_info/2,
			handle_cast/2,
			handle_call/3,
			code_change/3]).

%Bedre navn????
--record (box, {field = value}).

%	gen_server functions
init()
terminate()
handle_info()
handle_cast()
handle_call()

code_change(_OldVsn, State, _Extra) ->
%% No change planned. The function is there for the behaviour,
{ok, State}.