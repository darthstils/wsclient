-module(wsclient).

-export([
	start/0,
	stop/0,
	load_config/0,
	info/2,
	err/2
]).

-define(ConfigPathError,"Path to application config not found. Please check wsclient section in sys.config [{config,$PATH}]").
-define(ConfigPath,	"Loading application config ... Please wait ...").
-define(ConfigLoaded,	"Application config successfully loaded. Storing data, please wait ...").
-define(ConfigError,	"Application config not loaded. Please check config. Reason[~p]").
-define(ConfigStored,	"Application config successfully stored in memory.").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start() -> 	

	start_applications(
		file:consult(
			"relx.config"
		)
	).
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_applications({ok,[Config|_]}) ->

	lists:map(fun(Name) ->

		application:start(Name)

	end, erlang:element(3,Config));

start_applications(_) -> [].
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
stop() -> application:stop(polbot).
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
load_config() ->
	
	load_config(
		application:get_env(
			?MODULE,
			config
		),
		path
	).

load_config(undefined,_) 	-> err(?ConfigPathError,[]);
load_config({ok,Path},path) -> 

	info(?ConfigPath,[]),

	load_config(
		file:consult(Path),
		config
	);

load_config({error,Reason},config)		-> err(?ConfigError,[Reason]);
load_config({ok,[Parameters]},config)	->

	info(?ConfigLoaded,[]),

	[
		storage:set_items(Table,Values) || 
		{Table,Values} <- Parameters
	],

	info(?ConfigStored,[]).
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
info(Text,Args)	->	

	Info = binary_to_list(
		iolist_to_binary(
			io_lib:format(Text,Args)
		)
	),

	error_logger:info_report(Info).
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
err(Text,Args)-> 

	Error = binary_to_list(
		iolist_to_binary(
			io_lib:format(Text,Args)
		)
	),

	error_logger:error_report(Error).