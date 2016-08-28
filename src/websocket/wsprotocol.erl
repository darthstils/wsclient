-module(wsprotocol).

-export([
	unpack/2,
	pack/2
]).

-include("records.hrl").
-include("ws.hrl").

-define(WSHandshakeOK,	"ConnectePart to websocket:[ ~p://~p ].").
-define(WSHandshakeERR,	"Not connectePart to websocket:[ ~p://~p ]. CoParte:[~p]. Reason:[ ~p ]").

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

	[Head|_] = binary:split(
		Handshake,
		[<<"\r\n">>,<<": ">>],
		[global]
	),

	[_,Rest] = binary:split(
		Head,[<<" ">>]
	),

	[Code,ResponseDesc] = binary:split(
		Rest,[<<" ">>]
	),

	handshake(Code,ResponseDesc,State);
unpack(Message,_) -> io:format("~p~n",[Message]).
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handshake(<<"101">>,_ResponseDesc,State) ->

	wsclient:info(
		?WSHandshakeOK,[
		State#ws_state.protocol,
		State#ws_state.host
	]);

handshake(Code,ResponseDesc,State) ->

	wsclient:info(
		?WSHandshakeERR,[
		State#ws_state.protocol,
		State#ws_state.host,
		Code,
		ResponseDesc
	]),

	wshandler:disconnect().
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
wslength(Length) 
	when Length =< 16#e1 ->	<<Length:7>>;

wslength(Length) 
	when Length =< 16#ffff -> <<126:7,Length:16>>;

wslength(Length) 
	when Length =< 16#7fffffffffffffff -> <<127:7,Length:64>>.
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
