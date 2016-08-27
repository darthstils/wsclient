
-define(TCP_PORT(X),
	case X of
		"wss"	->	843;
		_	->	80
	end
).

-define(TCP_LIB(X),
	case X of
		"wss"	->	ssl;
		_	->	gen_tcp
	end
).