
-define(TCP_LIB(X),
	case X of
		"wss"	->	ssl;
		_	->	gen_tcp
	end
).

-define(WS_HANDSHAKE(Host),
	"GET / HTTP/1.1\r\n"
	"Host: "++Host++"\r\n"
	"Upgrade: websocket\r\n"
	"Connection: Upgrade\r\n"
	"Sec-WebSocket-Key: "
	++convert:to_list(
		base64:encode(
			crypto:rand_bytes(16)
		)
	)++"\r\n"
	"Sec-WebSocket-Version: 13\r\n"
	"\r\n"
).

-define(OPCODE(X),
	case X of
		continuation -> 0;
		text -> 1;
		binary -> 2;
		close -> 8;
		ping -> 9;
		pong -> 10;
		_ ->	9
	end	
).
