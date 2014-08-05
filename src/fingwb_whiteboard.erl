-module(fingwb_whiteboard).

-export([init/1, create/1]).

-record(board, {
	id,
	epoch
}).

-record(watcher, {
	pid,
	board_id
}).

-define(Tables, [
	{board,   [{attributes, record_info(fields, board)}]},
	{watcher, [{attributes, record_info(fields, watcher)}]}
]).

init([]) ->
	Node = node(),
	ok = case mnesia:create_schema([Node]) of
		ok -> ok;
		{error, {Node, {already_exists, Node}}} -> ok;
		_ -> error
	end,
	ok = mnesia:start(),
	CreateTable = fun({Table, Opts}) ->
		case mnesia:create_table(Table, Opts) of
			{atomic, ok} -> ok;
			{aborted, {already_exists, Table}} -> ok;
			{aborted, Reason} -> Reason
		end
	end,
	[ok = CreateTable(Table) || Table <- ?Tables ],
	ok.

create(Id) when is_binary(Id) ->
	{atomic, ok} = mnesia:transaction(fun()->
		mnesia:write({board, Id, timestamp()})
	end),
	ok.


timestamp() -> {Mega, Secs, Micro} = erlang:now(),  Mega*1000*1000*1000*1000 + Secs * 1000 * 1000 + Micro.
