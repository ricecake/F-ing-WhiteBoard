-module(fingwb_canvas).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
	{WhiteBoardId, Req2} = cowboy_req:binding(whiteboard_id, Req),
	{Code, Module} = case fingwb_whiteboard:exists(WhiteBoardId) of
		true  -> {200, whiteboard_dtl};
		false -> {404, notFound_dtl}
	end,
	{ok, Body} = Module:render([{whiteboard_id, WhiteBoardId}]),
	{ok, Reply} = cowboy_req:reply(Code, [], Body, Req2),
	{ok, Reply, State}.

terminate(_Reason, _Req, _State) -> ok.
