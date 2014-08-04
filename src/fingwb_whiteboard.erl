-module(fingwb_whiteboard).

-export([init/1]).

-record(board, {
	id
	epoch
}).

-record(watcher, {
	pid
	board_id
}).

-define(tables, [
	{board,   [{attributes, record_info(fields, board)}]},
	{watcher, [{attributes, record_info(fields, watcher)}]},
]).

init([]) ->
	ok = case mnesia:create_schema([node()]) of
		ok -> ok;
		{error, {node(), {already_exists, node()}}} -> ok;
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
	[ok = CreateTable(Table) || Table <- ?tables],
	ok.
