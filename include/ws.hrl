
-define(TCP_LIB(X),
	case X of
		"wss"	->	ssl;
		_	->	gen_tcp
	end
).

-define(WS_HANDSHAKE(Host,Subprotocol),
	"GET / HTTP/1.1\r\n"
	"Host: "++Host++"\r\n"
	"Upgrade: websocket\r\n"
	"Connection: Upgrade\r\n"
	"Sec-WebSocket-Key: "
	++ convert:to_list(
		base64:encode(
			crypto:rand_bytes(16)
		)
	) ++ "\r\n"
	"Sec-WebSocket-Version: 13\r\n"++
	case Subprotocol of
		[] -> "";
		_ ->
			"Sec-WebSocket-Protocol: " ++ Subprotocol ++ "\r\n"
	end ++ "\r\n"
).

-define(TO_OPCODE(X),
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

-define(FROM_OPCODE(X),
	case X of
		0 -> continuation;
		1 -> text;
		2 -> binary;
		8 -> close;
		9 -> ping;
		10 -> pong;
		_ ->	9
	end	
).
