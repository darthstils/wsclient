
-record(ws_state,{
	pid 							= null,
	protocol					= "ws",
	host							= "echo.websocket.org",
	reconnect_tries 	= 5,
	reconnect_timeout = 1000,
	num_of_tries			= 0
}).