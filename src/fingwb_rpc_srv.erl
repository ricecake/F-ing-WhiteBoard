-module(fingwb_rpc_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Pid) ->
    gen_server:start_link(?MODULE, Pid, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Owner) ->
    {ok, Owner}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({process, JSON}, Owner) ->
	Reason = try jiffy:decode(JSON) of
		{Entries} when is_list(Entries) ->
			[handleTask(Item, Owner) || Item <- Entries],
			normal
	catch
		_ -> inapplicable
	end,
	{stop, Reason, Owner};

handle_cast(_Msg, State) ->
    {stop, error, State}.

handle_info(_Info, State) ->
    {stop, error, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handleTask(Job, Owner) ->
	{ok, Worker} = supervisor:start_child(fingwb_rpc_worker_sup, [Owner]),
	gen_server:cast(Worker, Job).

