-module(fingwb_index).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
	Boards = fingwb_whiteboard:list(),
	{ok, Body} = index_dtl:render([{boards, Boards}]),
	{ok, Reply} = cowboy_req:reply(200, [], Body, Req),
	{ok, Reply, State}.

terminate(_Reason, _Req, _State) -> ok.

