-module(fingwb_create).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
	{ok, Reply} = cowboy_req:reply(302, [{<<"Location">>, <<"/", (getNewId())/binary >>}],
		<<"Creating New WhiteBoard...">>, Req),
	{ok, Reply, State}.

terminate(_Reason, _Req, _State) -> ok.

getNewId() -> erlang:integer_to_binary(binary:decode_unsigned(crypto:rand_bytes(8)), 36).
