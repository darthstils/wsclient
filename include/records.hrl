
-record(ws_state,{
	pid 			= null,
	protocol		= "wss",
	host			= "web.com",
	port 			= 80,
	subprotocol	=	[],
	receiver = receiver,
	connect_timeout		= 1000,
	reconnect_tries		= 5,
	reconnect_timeout	= 1000,
	num_of_tries		= 0
}).