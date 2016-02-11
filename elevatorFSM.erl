-module (elevatorFSM).
-compile(export_all).
%-behaviour (gen_fsm).


start() -> spawn(fun() -> init() end).

moveDown(Pid) -> Pid ! moveDown.

moveUp(Pid) -> Pid ! moveUp.

init() -> 
	driver ! setMo 	
