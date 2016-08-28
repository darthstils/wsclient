-module(receiver).

-include("records.hrl").

-export([
	message/2
]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
message({system,handshake_ok},_State) ->

	wshandler:send({<<"kyky">>,binary});

message({system,ping},_State) ->

	wshandler:send({<<>>,pong});

message({system,close},_State) ->

	wshandler:disconnect();

message(Message,_State) ->

	io:format("~p~n",[Message]).
