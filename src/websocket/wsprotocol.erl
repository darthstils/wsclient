-module(wsprotocol).

-export([
	unpack/2,
	pack/2
]).

-include("records.hrl").
-include("ws.hrl").

-define(WSHandshakeOK,	"Connected to websocket:[ ~p://~p ].").
-define(WSHandshakeERR,	"Not connected to websocket:[ ~p://~p ]. Code:[~p]. Reason:[ ~p ]").
-define(WSPing,	"Web Socket received PING message from:[ ~p://~p ].").
-define(WSClose,	"Web Socket received CLOSE message from:[ ~p://~p ].").
-define(WSPong,	"Web Socket sending PONG message to:[ ~p://~p ].").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
pack(Message,Type) 
		when is_binary(Message) ->
	
	Opcode = ?OPCODE(Type),

	Length = wslength(
		byte_size(Message)
	),

	Key = crypto:rand_bytes(4),

	<<MaskKey:32>> = Key,

	Head = <<1:1,0:3,Opcode:4,1:1,Length/bits,Key/bits>>,

	Masked = mask_message(
		MaskKey,Message,<<>>
	),

	<<Head/binary,Masked/binary>>;

pack(_,_) -> pack(<<>>,binary).
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
unpack(<<$H,$T,$T,$P,_/binary>>=Handshake,State) ->
	
	handshake(
		erlang:decode_packet(
			http_bin,Handshake,[]
		),
	State);
	
unpack(<<1:1,0:3,Opcode:4,0:1,_Len:7,_Rest/bits>>,State) 
		when Opcode == 9 -> 

	wsclient:info(
		?WSPing,[
		State#ws_state.protocol,
		State#ws_state.host
	]),	

	wsclient:info(
		?WSPong,[
		State#ws_state.protocol,
		State#ws_state.host
	]),	

	wshandler:send({<<>>,pong});

unpack(<<1:1,0:3,Opcode:4,0:1,_Len:7,_Rest/bits>>,State) 
		when Opcode == 8 -> 

	wsclient:info(
		?WSClose,[
		State#ws_state.protocol,
		State#ws_state.host
	]),

	wshandler:disconnect();

unpack(<<1:1,0:3,_Opcode:4,0:1,126:7,_Len:16,_Rest/bits>>,_) ->

	io:format("~p~n",[_Rest]);

unpack(<<1:1,0:3,_Opcode:4,0:1,_Len:7,_Rest/bits>>,_) ->
	
	io:format("~p~n",[_Rest]).
	
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handshake({ok,{_,_,101,_},_},State) ->

	wsclient:info(
		?WSHandshakeOK,[
		State#ws_state.protocol,
		State#ws_state.host
	]);

handshake({ok,{_,_,Code,Desc},_},State) ->

	wsclient:info(
		?WSHandshakeERR,[
		State#ws_state.protocol,
		State#ws_state.host,
		Code,
		Desc
	]),

	wshandler:disconnect().
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
wslength(Length) when Length =< 16#7f -> <<Length:7>>;

wslength(Length) when Length =< 16#ffff -> <<126:7,Length:16>>;

wslength(Length) when Length =< 16#7fffffffffffffff -> <<127:7,Length:64>>.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
mask_message(_,<<>>,Acc) -> Acc;
mask_message(Key, << Part:32, Rest/bits >>, Acc) ->
    T = Part bxor Key,
    mask_message(Key, Rest, << Acc/binary, T:32 >>);
mask_message(Key, << Part:24 >>, Acc) ->
    << KeyPart:24, _:8 >> = << Key:32 >>,
    T = Part bxor KeyPart,
    << Acc/binary, T:24 >>;
mask_message(Key, << Part:16 >>, Acc) ->
    << KeyPart:16, _:16 >> = << Key:32 >>,
    T = Part bxor KeyPart,
    << Acc/binary, T:16 >>;
mask_message(Key, << Part:8 >>, Acc) ->
    << KeyPart:8, _:24 >> = << Key:32 >>,
    T = Part bxor KeyPart,
		<< Acc/binary, T:8 >>.
