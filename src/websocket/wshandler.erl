-module(wshandler).

-behaviour(gen_server).

-include("ws.hrl").
-include("records.hrl").

-define(Connecting,	"Connecting to host:[ ~p ] ... Please wait ...").
-define(NotConnected,	"Connection not set with host:[ ~p ]. Reason:[ ~p ].").
-define(Connected,	"Connection set with host:[ ~p ].").
-define(Reconnecting,	"Start reconnrction process with host:[ ~p ].").
-define(Disconnect,	"Connection with host:[ ~p ] closed.").
-define(WSHandshake,	"Connecting to websocket:[ ~p://~p ] ... Please wait ...").

-export([start_link/0]).

-export([
	init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-export([
	send/1,
	connect/0,
	disconnect/0,
	reconnect/0,
	reload_config/0
]).

-export([
	send/2,
	connect/2,
	disconnect/2,
	tcp_closed/2,
	ssl_closed/2,
	reconnect/2,
	reload_config/2
]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
send(Message)	-> ?MODULE ! {send,Message}.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
connect()	-> ?MODULE ! {connect,[]}.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
disconnect()	-> ?MODULE ! {disconnect,[]}.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
reconnect()	-> ?MODULE ! {reconnect,[]}.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
reload_config()	-> ?MODULE ! {reload_config,[]}.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link() ->

    gen_server:start_link(
    	{local, ?MODULE}, ?MODULE, [], []
    ).
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init([]) -> 

	?MODULE ! {connect,[]},

	{ok,record:from_proplist(
		storage:get_all(?MODULE),
		#ws_state{}
	)}.
%%--------------------------------------------------------------------
%%
%%-------------------------- ------------------------------------------
handle_call(_Request, _From, State) -> {reply, ignored, State}.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_info({Command,Args}, State) -> 
	
	{noreply, ?MODULE:Command(
		Args,State
	)};

handle_info(_Info, State) -> io:format("~p~n",[State]),{noreply, State}.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
send(_,#ws_state{
		pid = null 
	} = State) -> State;

send({Message,Type},State) ->

	% io:format("~p~n",[Message]),

	send(
		wsprotocol:pack(
			Message,Type
		),State
	);

send(Message,State) ->

	?TCP_LIB(
		State#ws_state.protocol
	):send(
		State#ws_state.pid,
		Message
	),

	State.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
connect(_Args,#ws_state{
		pid = null
	}=State) ->
	
	wsclient:info(?Connecting,[
		State#ws_state.host
	]),

	Result = ?TCP_LIB(
		State#ws_state.protocol
	):connect(
		State#ws_state.host,
		State#ws_state.port,
		[
			{mode, 		binary},
      {active, 	false},
      {packet, 	raw}
    ],
	State#ws_state.connect_timeout),

	connect(_Args,State#ws_state{
		pid = Result
	});

connect(_Args,#ws_state{
		pid = {ok,PID}
	}=State) ->

	wsclient:info(?Connected,[
		State#ws_state.host
	]),

	spawn(fun()->
		receiver(State#ws_state{
				pid = PID
			})
	end),

	wsclient:info(
		?WSHandshake,[
		State#ws_state.protocol,
		State#ws_state.host
	]),

	send(?WS_HANDSHAKE(
		State#ws_state.host,
		State#ws_state.subprotocol
	)),

	State#ws_state{ pid = PID };

connect(_Args,#ws_state{
		pid = {error,Reason}
	}=State) ->

	wsclient:err(?NotConnected,[
		State#ws_state.host,
		Reason
	]),

	State#ws_state{ pid = null };

connect(_,State) -> State.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
ssl_closed(_Args,State) -> disconnect(_Args,State).
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tcp_closed(_Args,State) -> disconnect(_Args,State).
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
disconnect(_Args,#ws_state{
		pid = null
	}=State) -> State;
disconnect(_Args,#ws_state{
		pid = PID
	}=State) ->

	wsclient:info(?Disconnect,[
		State#ws_state.host
	]),

	?TCP_LIB(
		State#ws_state.protocol
	):close(PID),

	State#ws_state{	pid = null }.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
reconnect(_Args,State) -> 

	wsclient:info(?Reconnecting,[
		State#ws_state.host
	]),

	connect(_Args,disconnect(
			_Args,State
		)
	).
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
reload_config(_Args,State) -> 

	disconnect(
		_Args,State
	),

	connect(_Args,
		record:from_proplist(
			storage:get_all(?MODULE),
			#ws_state{}
		)
	).	
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
receiver(State) ->

	receiver(
		State,
		?TCP_LIB(
			State#ws_state.protocol
		):recv(State#ws_state.pid,0),
		message
	).

receiver(_,{error,_Reason},message) -> [];
receiver(State,{ok,Message},message) -> 

	wsprotocol:unpack(
		Message,State
	),

	receiver(State).


