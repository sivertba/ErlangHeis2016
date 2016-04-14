%this is the record for order
-record (order, {floor, direction}).

%cross-module macros
-define(NUMBER_OF_FLOORS, 4).

%elevator macros
-define(STATE_MONITOR, state_monitor).
-define(FSM_PID, fsm).


%order_handler macros
-define(QUEUE_PID, queue).
-define(DETS_TABLE_NAME, "ordersETS").

%elev_dirver macros
-define(BUTTON_TYPES, [up, down, command]).
-define(POLL_PERIOD, 50).

%elev_FSM macros
-define (DOOR_OPEN_DURATION, 2000).
-define (EXPECTED_MAX_TIME_BETWEEN_FLOORS, 4000).