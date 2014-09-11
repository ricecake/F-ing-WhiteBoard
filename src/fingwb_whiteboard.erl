-module(fingwb_whiteboard).

-export([init/1, create/0, exists/1, delete/1, watch/1, unWatch/1, publish/2, notify/2, watchers/1, list/0, readArchive/1, clear/1]).

-record(board, {
	id,
	timer
}).

-record(watcher, {
	pid,
	board_id
}).

-record(archive, {
	id,
	board_id,
	data
}).

-define(Tables, [
	{board,   [{attributes, record_info(fields, board)}]},
	{watcher, [{attributes, record_info(fields, watcher)}]},
	{archive, [{attributes, record_info(fields, archive)}]}
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
	{atomic, {ok, Id} = Result} = mnesia:transaction(fun creator/0),
	ok = setTimeout(Id),
	Result.

creator() ->
	Id = getNewId(),
	case mnesia:read({board, Id}) of
		[] ->
			ok = mnesia:write(#board{id=Id, timer=undefined}),
			{ok, Id};
		_  -> creator()
	end.

exists(Id) when is_binary(Id) ->
	{atomic, Result} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[] -> false;
			_  -> true
		end
	end),
	Result.

delete(Id) when is_binary(Id) ->
	{atomic, Result} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[] -> ok;
			_  ->
				case mnesia:match_object(#watcher{ pid='_', board_id=Id}) of
					[] ->
						[ok = mnesia:delete_object(Row) || Row <- mnesia:match_object(#archive{board_id=Id, _='_'})],
						ok = mnesia:delete({board, Id});
					_  -> in_use
				end
		end
	end),
	Result.

watch(Id) when is_binary(Id) ->
	clearTimeout(Id),
	{atomic, Result} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[]    -> undefined;
			[_Row] -> mnesia:write(#watcher{pid=self(), board_id=Id})
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
	ok = case watchers(Id) of
		[] -> setTimeout(Id);
		_  -> ok
	end,
	Result.

publish(Id, Message) when is_binary(Id) ->
	{ok, ZipMsg} = snappy:compress(erlang:term_to_binary(Message)),
	{atomic, Watchers} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[] -> undefined;
			_  ->
				ok = mnesia:write(#archive{id={Id, timestamp(), getNewId()}, board_id=Id, data=ZipMsg }),
				mnesia:match_object(#watcher{ pid='_', board_id=Id})
		end
	end),
	[Pid ! Message || #watcher{pid=Pid} <- Watchers],
	ok.

notify(Id, Message) when is_binary(Id) ->
	{atomic, Watchers} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[] -> undefined;
			_  -> mnesia:match_object(#watcher{ pid='_', board_id=Id})
		end
	end),
	[Pid ! Message || #watcher{pid=Pid} <- Watchers],
	ok.

readArchive(Id) when is_binary(Id) ->
	{atomic, Archive} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[] -> undefined;
			_  -> mnesia:match_object(#archive{ board_id=Id, _='_' })
		end
	end),
	[ erlang:binary_to_term(Zipped) || {ok, Zipped} <-[
		snappy:decompress(Data) || #archive{data=Data} <- lists:sort(fun(#archive{id=Aid}, #archive{id=Bid})-> Bid >= Aid end, Archive)]
	].

watchers(Id) when is_binary(Id) ->
	{atomic, Watchers} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[] -> undefined;
			_  -> mnesia:match_object(#watcher{ pid='_', board_id=Id})
		end
	end),
	[ Pid || #watcher{pid=Pid} <- Watchers].

clear(Id) when is_binary(Id) ->
	{atomic, Result} = mnesia:transaction(fun()->
		[ok = mnesia:delete_object(Row) || Row <- mnesia:match_object(#archive{board_id=Id, _='_'})],
		ok
	end),
	Result.

list() ->
	{atomic, Boards} = mnesia:transaction(fun()->
		mnesia:foldl(fun(#board{id=Id}, Acc)-> [Id|Acc] end, [], board)
	end),
	Boards.

setTimeout(Id) when is_binary(Id) ->
	{ok, Tref} = timer:apply_after(10000, fingwb_whiteboard, delete, [Id]),
	{atomic, _Result} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[]    -> undefined;
			[Row] -> mnesia:write(Row#board{timer=Tref})
		end
	end),
	ok.

clearTimeout(Id) when is_binary(Id) ->
	{atomic, Result} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[]    -> undefined;
			[Row] -> Row#board.timer
		end
	end),
	timer:cancel(Result).


getNewId() -> erlang:integer_to_binary(binary:decode_unsigned(crypto:rand_bytes(8)), 36).
timestamp() -> {Mega, Secs, Micro} = erlang:now(),  Mega*1000*1000*1000*1000 + Secs * 1000 * 1000 + Micro.
