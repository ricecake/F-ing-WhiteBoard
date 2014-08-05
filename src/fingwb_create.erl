-module(fingwb_create).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
	{ok, {NewBoard, _TimeStamp}} = fingwb_whiteboard:create(),
	{ok, Reply} = cowboy_req:reply(302, [{<<"Location">>, <<"/", (NewBoard)/binary >>}],
		<<"Creating New WhiteBoard...">>, Req),
	{ok, Reply, State}.

terminate(_Reason, _Req, _State) -> ok.

