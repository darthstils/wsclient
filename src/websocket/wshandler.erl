-module(wshandler).

-behaviour(gen_server).

-include("ws.hrl").
-include("records.hrl").

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
	connect/2
]).

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

handle_info(_Info, State) -> 

	io:format("~p~n",[State]),

	{noreply, State}.
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
connect(_Args,#ws_state{
		pid = null
	}=State) ->
		
	Result = ?TCP_LIB(
		State#ws_state.protocol
	):connect(
		State#ws_state.host,
		State#ws_state.port,
		[binary,{packet,0}],
	State#ws_state.connect_timeout),

	connect(_Args,State#ws_state{
		pid = Result
	});

connect(_Args,#ws_state{
		pid = {ok,PID}
	}=State) ->

	State#ws_state{ pid = PID };

connect(_Args,#ws_state{
		pid = {error,Reason}
	}=State) ->

	State#ws_state{ pid = null };

connect(_,State) -> State.
