%%%-------------------------------------------------------------------
%%% @author phyzX
%%% @copyright (C) 2015, PJSC CB PrivatBank
%%% @doc
%%% <div style="background:#eee">
%%%	<b><i>Record convertation module.</i></b>
%%%	<br></br>
%%% </div>
%%% @end
%%% Created : 21. Nov 2015 12:46 PM
%%%-------------------------------------------------------------------
-module(record).
-author("DarthSTILS").

% API
-export([
	to_proplist/1,
	to_proplist/2,
	from_proplist/2
]).

-include("records.hrl").

-define(RECSINFO,[
	{ws_state,						record_info(fields,ws_state)}
]).

%%--------------------------------------------------------------------
%% @doc
%% Create proplist from incoming keys of record.
%% @end
%%--------------------------------------------------------------------
to_proplist(Keys,Record) -> get_value(Keys,Record,[],proplist).
%%--------------------------------------------------------------------
%% @doc
%% Convert all values of <b>existing</b> Erlang record to proplist(list of keys::binary() and values).
%% @end
%%--------------------------------------------------------------------
to_proplist(Record) -> get_value(fields(Record),Record,[],proplist).
%%--------------------------------------------------------------------
%% @private
%% Convert proplist to <b>existing</b> Erlang record.
%% @end
%%--------------------------------------------------------------------
from_proplist([],Record) -> Record;

from_proplist([
		{Key,Value}|Rest
	],Record) -> 
	
	AtomKey = convert:to_atom(Key),

	from_proplist(Rest,
		set_value(
			AtomKey,
			Value,
			Record
		)
	);

from_proplist(_,Record) ->	

	from_proplist([],Record).
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
set_value(Key,Value,Record) 
	when is_atom(Key) -> 

	set_value(
		field_index(
			Key,Record
		),
		Value,Record
	);

set_value(Index,Value,Record) 
	when is_integer(Index) 
	andalso Index > 0 
	andalso Index < size(Record) + 1 ->
	
	erlang:setelement(
		Index,
		Record,
		Value
	);

set_value(_,_,Record) -> Record.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Getting keys and values from record. Creating required type of return.
%% @end
%%--------------------------------------------------------------------
get_value([],_,Acc,_) -> Acc;
get_value([Key|Rest],Record,Acc,proplist) ->

	Index = field_index(
		convert:to_atom(Key),
		Record
	),

	Value = case Index > 0
		andalso 
			Index < size(Record) of 

		false -> <<>>;

		true 	-> erlang:element(Index,Record)

	end,

	convert:to_bin(Key),

	get_value(Rest,Record,
		Acc ++ [{
			convert:to_bin(Key),
			Value
		}],
	proplist).
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fields indexes.
%% @end
%%--------------------------------------------------------------------
field_index(Field,Record) ->
	length(
		lists:takewhile(fun(Name) ->
				Name /= Field
		end, fields(Record))
	) + 2.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% All fields from record.
%% @end
%%--------------------------------------------------------------------
fields(Record) ->
	proplists:get_value(
		erlang:element(1,Record),
		?RECSINFO
	).