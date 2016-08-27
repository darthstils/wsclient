-module(wsprotocol).

-export([
	unpack/2
]).

-include("records.hrl").

-define(WSHandshakeOK,	"Connected to websocket:[ ~p://~p ].").
-define(WSHandshakeERR,	"Not connected to websocket:[ ~p://~p ]. Code:[~p]. Reason:[ ~p ]").

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
