%this is the record for order
-record (order, {floor,direction,timestamp = erlang:timestamp()}).

%cross-module macros
-define(NUMBER_OF_FLOORS, 4).

%order_handler macros
-define(QUEUE_PID, queue).
-define(DETS_TABLE_NAME, "ordersETS").

%elev_dirver macros
-define(BUTTON_TYPES, [up, down, command]).
-define(POLL_PERIOD, 50).