-module(fingwb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    fingwb_whiteboard:init([]),
    {ok, _Pid} = fingwb_sup:start_link(),
    Dispatch = cowboy_router:compile([
	    {'_', [
			{"/", cowboy_static, {priv_file, fingwb, "index.html"}},
			{"/new", fingwb_create, []},
			{"/ws/:whiteboard_id", fingwb_msg_handler, []},
			{"/:whiteboard_id", fingwb_canvas, []},
			{"/js/[...]", cowboy_static, {priv_dir, fingwb, "static/js/"}},
			{"/css/[...]", cowboy_static, {priv_dir, fingwb, "static/css/"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 25, [{ip, {127,0,0,1}}, {port, 8080}],
        [{env, [{dispatch, Dispatch}]}]).

stop(_State) ->
    ok.
