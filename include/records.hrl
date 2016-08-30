
-record(ws_state,{
	pid 			= null,
	protocol		= "wss",
	host			= "web.com",
	port 			= 80,
	subprotocol	=	[],
	receiver = receiver,
	connect_timeout		= 1000
	
}).