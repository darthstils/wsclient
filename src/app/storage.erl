-module(storage).

-export([
	set_items/2,
	get_all/1
]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
get_all(Table) -> 

	case ets:info(Table) of

		undefined -> [];

		_ ->
		 
			lists:append(
				ets:match(Table,'$1')
			)

	end.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
set_items(Table,Items) ->

	existens_of_table(Table),

	ets:insert(Table,Items);

set_items(_,_) -> [].
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
existens_of_table(Table) ->
	
	existens_of_table(
		ets:info(Table),
		Table
	).

existens_of_table(undefined,Table) ->

	ets:new(Table,
		[
			public,
			named_table
		]
	);

existens_of_table(_,Table) -> 

	ets:delete_all_objects(Table).
