-module(fingwb_msg_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(ws_state, {
	id
}).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	{WhiteBoardId, Req2} = cowboy_req:binding(whiteboard_id, Req),
	ok = fingwb_whiteboard:watch(WhiteBoardId),
        ok = fingwb_whiteboard:publish(WhiteBoardId, {join, self()}),
[ self() ! {join, Pid}|| Pid <- fingwb_whiteboard:watchers(WhiteBoardId)],
	{ok, Req2, #ws_state{id=WhiteBoardId}}.

websocket_handle({text, Msg}, Req, State) ->
	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({join, Pid}, Req, State) ->
	{reply, {text, jiffy:encode({[{<<"join">>, list_to_binary(pid_to_list(Pid))}]})}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, #ws_state{id=WbId}) ->
	fingwb_whiteboard:unWatch(WbId),
        fingwb_whiteboard:publish(WbId, {leave, self()}).
