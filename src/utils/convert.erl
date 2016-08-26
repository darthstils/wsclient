%%%-------------------------------------------------------------------
%%% @author phyzX
%%% @copyright (C) 2015, PJSC CB PrivatBank
%%% @doc
%%% <div style="background:#eee">
%%%	<b><i>Types convertation module.</i></b>
%%% </div>
%%% @end
%%% Created : 21. Nov 2015 12:46 PM
%%%-------------------------------------------------------------------
-module(convert).
-author("phyzx").

%% API
-export([
	ip2str/1,
	ip2bin/1,
	bin2ip/1,
	str2ip/1,
	to_list/1,
	to_int/1,
	to_atom/1,
	to_bin/1
]).
%%--------------------------------------------------------------------
%% @doc
%% Erlang ip {1,1,1,1} to String ip ("1.1.1.1").
%% @end
%%--------------------------------------------------------------------
ip2str(IP) ->

	lists:append(
		lists:zipwith(fun(A,B) -> A ++ B end, 
			[ to_list(X) || X <- tuple_to_list(IP) ],
			[".",".",".",""]
		)
	).
%%--------------------------------------------------------------------
%% @doc
%% Erlang ip {1,1,1,1} to Binary ip (<<"1.1.1.1">>).
%% @end
%%--------------------------------------------------------------------
ip2bin(IP) -> 

	to_bin(
		ip2str(IP)
	).
%%--------------------------------------------------------------------
%% @doc
%% Binary ip (<<"1.1.1.1">>) to Erlang ip {1,1,1,1}
%% @end
%%--------------------------------------------------------------------
-spec bin2ip(binary()) -> term().
bin2ip(BinIP) ->

	[A,B,C,D] = binary:split(
		BinIP,<<".">>,[global]
	),

	{
		to_int(A),
		to_int(B),
		to_int(C),
		to_int(D)
	}.
%%--------------------------------------------------------------------
%% @doc
%% Sting ip ("1.1.1.1") to Erlang ip {1,1,1,1}
%% @end
%%--------------------------------------------------------------------
-spec str2ip(list()) -> term().
str2ip(StrIP) ->

	[A,B,C,D] = string:tokens(
		StrIP,"."),

	{
		to_int(A),
		to_int(B),
		to_int(C),
		to_int(D)
	}.
%%--------------------------------------------------------------------
%% @doc
%% Any to integer.	  
%% @end
%%--------------------------------------------------------------------
-spec to_int(any()) -> integer().
to_int(X)
	when is_integer(X) -> X;

to_int(X)
	when is_atom(X) ->

	list_to_integer(
		binary_to_list(
			atom_to_binary(X,latin1)
		)
	);

to_int(X)
	when is_binary(X) ->

	list_to_integer(
		binary_to_list(X)
	);

to_int(X)
	when is_list(X) ->

	list_to_integer(X);

to_int(X)
	when is_float(X) ->

	trunc(X).
%%--------------------------------------------------------------------
%% @doc
%% Any to Erlang Atom.
%% @end
%%--------------------------------------------------------------------
-spec to_atom(any()) -> atom().
to_atom(X)
	when is_integer(X) ->

	to_atom(
		integer_to_binary(X)
	);

to_atom(X)
	when is_list(X) ->

	to_atom(
		list_to_binary(X)
	);

to_atom(X)
	when is_list(X) ->

	to_atom(
		list_to_binary(X)
	);

to_atom(X)
	when is_float(X) ->

	to_atom(
		iolist_to_binary(
			io_lib:format("~.2f",[X])
		)
	);

to_atom(X) 
	when is_binary(X) -> 

	binary_to_atom(X,latin1);

to_atom(X) -> X.
%%--------------------------------------------------------------------
%% @doc
%% Any to binary.	
%% @end
%%--------------------------------------------------------------------
-spec to_bin(any()) -> binary().
to_bin(X)
	when is_atom(X) ->

	atom_to_binary(X,latin1);

to_bin(X)
	when is_integer(X) ->

	to_bin(
		integer_to_list(X)
	);

to_bin(X)
	when is_float(X) ->
	
	iolist_to_binary(
		io_lib:format("~.2f",[X])
	);

to_bin(X)
	when is_list(X) ->

	list_to_binary(X);

to_bin(X) -> X.
%%--------------------------------------------------------------------
%% @doc
%% Any to list.	
%% @end
%%--------------------------------------------------------------------
-spec to_list(any()) -> list().
to_list(X)
	when is_atom(X) ->

	to_list(
		atom_to_binary(X,latin1)
	);

to_list(X)
	when is_integer(X) ->
	
	integer_to_list(X);

to_list(X)
	when is_float(X) ->
	
	to_list(
		iolist_to_binary(
			io_lib:format("~.2f",[X])
		)
	);

to_list(X)
	when is_binary(X) ->

	binary_to_list(X);

to_list(X) -> X.