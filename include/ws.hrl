
-define(TCP_LIB(X),
	case X of
		"wss"	->	ssl;
		_	->	gen_tcp
	end
).