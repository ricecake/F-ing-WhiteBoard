-module(fingwb_canvas).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
	{WhiteBoardId, Req2} = cowboy_req:binding(whiteboard_id, Req),
	{ok, Body} = whiteboard_dtl:render([{whiteboard_id, WhiteBoardId}]),
	{ok, Reply} = cowboy_req:reply(200, [], Body, Req2),
	{ok, Reply, State}.

terminate(_Reason, _Req, _State) -> ok.
