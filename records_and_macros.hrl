%this is the record for order
-record (order, {floor, direction, timestamp = erlang:now()}).

%cross-module macros
-define(NUMBER_OF_FLOORS, 4).
-define(DETS_TABLE_NAME, "ordersETS").

%elevator macros
-define(STATE_MONITOR_PID, state_monitor).
-define(FSM_PID, fsm).


%order_handler macros
-define(QUEUE_PID, queue).

%elev_dirver macros
-define(BUTTON_TYPES, [up, down, command]).
-define(POLL_PERIOD, 50).

%elev_FSM macros
-define (DOOR_OPEN_DURATION, 1000).
-define (EXPECTED_MAX_TIME_BETWEEN_FLOORS, 7000).