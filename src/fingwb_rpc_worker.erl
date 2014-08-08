-module(fingwb_rpc_worker).
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


handle_cast({Verb, Data}, Owner) ->
	ok = process(Verb, Data, Owner),
	{stop, normal, Owner};
handle_cast(_Msg, State) ->
	{stop, inapplicable, State}.

handle_call(_Request, _From, State) ->
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


process(_Verb, _Data, _Owner) -> ok.

subProcess(Verb, Data, Owner) when is_list(Data) ->
	{ok, Worker} = supervisor:start_child(fingwb_rpc_worker_sup, [Owner]),
	[ ok = gen_server:cast(Worker, {Verb, Job}) || Job <- Data ],
	ok.

