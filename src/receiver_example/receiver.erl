-module(receiver).

-include("records.hrl").

-export([
	message/3
]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
message({system,handshake_ok},_,_State) ->

	wshandler:send({<<"HELLO!">>,binary}),
	wshandler:send({<<"HELLO!">>,text}),

	[];

message({system,ping},_Reason,_State) ->

	wshandler:send({<<>>,pong});

message({system,close},_Reason,_State) ->

	wshandler:disconnect();

message(_,Message,_State) ->
	
	io:format("~p~n",[Message]).
