-module(fingwb_whiteboard).

-export([init/1, create/0, watch/1, unWatch/1, publish/2, watchers/1]).

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

create() ->
	{atomic, Result} = mnesia:transaction(fun creator/0),
	Result.

creator() ->
	Id = getNewId(),
	case mnesia:read({board, Id}) of
		[] ->
			TimeStamp = timestamp(),
			ok = mnesia:write(#board{id=Id, epoch=TimeStamp}),
			{ok, {Id, TimeStamp}};
		_  -> creator()
	end.

watch(Id) when is_binary(Id) ->
	{atomic, Result} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[] -> undefined;
			_  -> mnesia:write(#watcher{pid=self(), board_id=Id})
		end
	end),
	Result.

unWatch(Id) when is_binary(Id) ->
	{atomic, Result} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[] -> undefined;
			_  -> mnesia:delete({watcher, self()})
		end
	end),
	Result.

publish(Id, Message) when is_binary(Id) ->
	{atomic, Watchers} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[] -> undefined;
			_  -> mnesia:match_object(#watcher{ pid='_', board_id=Id})
		end
	end),
	[Pid ! Message || #watcher{pid=Pid} <- Watchers],
	ok.

watchers(Id) when is_binary(Id) ->
	{atomic, Watchers} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[] -> undefined;
			_  -> mnesia:match_object(#watcher{ pid='_', board_id=Id})
		end
	end),
	[ Pid || #watcher{pid=Pid} <- Watchers].

getNewId() -> erlang:integer_to_binary(binary:decode_unsigned(crypto:rand_bytes(8)), 36).
timestamp() -> {Mega, Secs, Micro} = erlang:now(),  Mega*1000*1000*1000*1000 + Secs * 1000 * 1000 + Micro.
