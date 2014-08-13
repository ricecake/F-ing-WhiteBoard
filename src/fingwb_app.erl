-module(fingwb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    fingwb_whiteboard:init([]),
    {ok, Pid} = fingwb_sup:start_link(),
    Dispatch = cowboy_router:compile([
	    {'_', [
			{"/", fingwb_index, []},
			{"/new", fingwb_create, []},
			{"/ws/:whiteboard_id", fingwb_msg_handler, []},
			{"/:whiteboard_id", fingwb_canvas, []},
			{"/js/[...]", cowboy_static, {priv_dir, fingwb, "static/js/"}},
			{"/css/[...]", cowboy_static, {priv_dir, fingwb, "static/css/"}},
			{"/images/[...]", cowboy_static, {priv_dir, fingwb, "static/images/"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 25, [{ip, {127,0,0,1}}, {port, 8080}],
        				[{env, [{dispatch, Dispatch}]}]),
	{ok, Pid}.

stop(_State) ->
    ok.
