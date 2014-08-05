-module(fingwb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
	    {'_', [
			{"/", fingwb_create, []},
			{"/ws/:whiteboard_id", fingwb_msg_handler, []},
			{"/:whiteboard_id", fingwb_canvas, []},
			{"/static/[...]", cowboy_static, {priv_dir, fingwb, "static"}}
		]}
	]),
    {ok, _} = cowboy:start_http(http, 25, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]),
    fingwb_sup:start_link().

stop(_State) ->
    ok.
