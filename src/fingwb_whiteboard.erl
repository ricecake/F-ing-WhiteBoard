-module(fingwb_whiteboard).

-export([init/1, create/0, exists/1, delete/1, watch/1, unWatch/1, publish/2, notify/2, watchers/1, readArchive/1, prune/0, clear/1]).

-record(board, {
	id,
	used=false,
	epoch
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
	{ok, _TRef} = timer:apply_interval(30000, fingwb_whiteboard, prune, []),
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
	{atomic, Result} = mnesia:transaction(fun()->
		case mnesia:read({board, Id}) of
			[]    -> undefined;
			[Row] ->
				mnesia:write(#watcher{pid=self(), board_id=Id}),
				mnesia:write(Row#board{used=true})
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
		[] -> {ok, _} = timer:apply_after(10000, fingwb_whiteboard, delete, [Id]), ok;
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
prune() ->
	mnesia:transaction(fun()->
		mnesia:foldl(
			fun(#board{id=Id, used=false}, Acc)->
				case watchers(Id) of
					[] ->
						delete(Id),
						Acc+1;
					_  -> Acc
				end;
			(#board{}, Acc)-> Acc
		end, 0, board)
	end).

getNewId() -> erlang:integer_to_binary(binary:decode_unsigned(crypto:rand_bytes(8)), 36).
timestamp() -> {Mega, Secs, Micro} = erlang:now(),  Mega*1000*1000*1000*1000 + Secs * 1000 * 1000 + Micro.
