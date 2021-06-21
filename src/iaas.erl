%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Manage Computers
%%% 
%%% Created : 
%%% -------------------------------------------------------------------
-module(iaas). 
 
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state, {running_hosts,not_available_hosts,
		running_clusters,not_available_clusters}).



%% --------------------------------------------------------------------
%% Definitions 
-define(WantedStateInterval,60*1000).

%% --------------------------------------------------------------------


-export([
	 status_all_hosts/0,
	 running_hosts/0,
	 not_available_hosts/0,
	 status_host/1
	]).

-export([
	 wanted_clusters/0,
	 create_cluster/1,
	 status_all_clusters/0,
	 running_clusters/0,
	 not_available_clusters/0,
	 status_cluster/1
	]).

-export([start/0,
	 stop/0,
	 ping/0,
	 wanted_state/2,
	 wanted_state/1,
	 clusters_is_wanted_state/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================


%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


ping()-> 
    gen_server:call(?MODULE, {ping},infinity).

%%-----------------------------------------------------------------------
status_all_hosts()->
    gen_server:call(?MODULE, {status_all_hosts},infinity).
running_hosts()->
    gen_server:call(?MODULE, {running_hosts},infinity).
not_available_hosts()->
    gen_server:call(?MODULE, {not_available_hosts},infinity).
status_host(HostId)->
    gen_server:call(?MODULE, {status_host,HostId},infinity).

%%-----------------------------------------------------------------------
wanted_clusters()->
    gen_server:call(?MODULE, {wanted_clusters},infinity).
create_cluster(ClusterId)->
    gen_server:call(?MODULE, {create_cluster,ClusterId},infinity).

status_all_clusters()->
    gen_server:call(?MODULE, {status_all_clusters},infinity).
running_clusters()->
    gen_server:call(?MODULE, {running_clusters},infinity).
not_available_clusters()->
    gen_server:call(?MODULE, {not_available_clusters},infinity).
status_cluster(ClusterId)->
    gen_server:call(?MODULE, {status_cluster,ClusterId},infinity).
clusters_is_wanted_state()->
    gen_server:call(?MODULE, {clusters_is_wanted_state},infinity).

%%----------------------------------------------------------------------

wanted_state(Interval)->
    gen_server:cast(?MODULE, {wanted_state,Interval}).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------

% To be removed

init([]) ->
    ssh:start(),
    case rpc:call(node(),host,status_all_hosts,[],15*1000) of
	{ok,RH,NAH}->
	    RunningHosts=RH,
	    NotAvailableHosts=NAH;
	_Err->
	    RunningHosts=[],
	    NotAvailableHosts=[]
    end,
    spawn(fun()->wanted_state(?WantedStateInterval,[]) end),
    {ok, #state{running_hosts=RunningHosts,
		not_available_hosts=NotAvailableHosts,
		running_clusters=[],
		not_available_clusters=[]}}.
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------

%%-------- Hosts
handle_call({status_all_hosts},_From,State) ->
    Reply={State#state.running_hosts,State#state.not_available_hosts},
    {reply, Reply, State};

handle_call({running_hosts},_From,State) ->
    Reply=State#state.running_hosts,
    {reply, Reply, State};

handle_call({not_available_hosts},_From,State) ->
    Reply=State#state.not_available_hosts,
    {reply, Reply, State};

handle_call({status_host,HostId},_From,State) ->
    AllHosts=lists:append(State#state.running_hosts,State#state.not_available_hosts),
    Reply=[{Status,XHostId,Ip,Port}||{Status,XHostId,Ip,Port}<-AllHosts,
			       HostId==XHostId],
    {reply, Reply, State};

%%------- Clusters
handle_call({clusters_is_wanted_state},_From,State) ->
    Reply=rpc:call(node(),cluster,is_wanted_state,[State#state.running_clusters]),
    {reply, Reply, State};

handle_call({wanted_clusters},_From,State) ->
    
    Reply=[ClusterId||{ClusterId,_KubeletNode,_NumWorkers,_WorkerNodes,_Cookie,_Glurk}<-db_cluster_info:read_all()],
    {reply, Reply, State};


handle_call({create_cluster,ClusterId},_From,State) ->
    Reply = case rpc:call(node(),cluster,create,[ClusterId],25*1000) of
		{ok,ClusterId,RunningKubeletNodes}->
		    RunningClusters=[{ClusterId,RunningKubeletNodes}|lists:keydelete(ClusterId,1,State#state.running_clusters)],
		    NewState=State#state{running_clusters=RunningClusters},
		    ok;
		Err->
		    NewState=State,
		    {error,[Err]}
	    end,
    {reply, Reply, NewState};

handle_call({status_all_clusters},_From,State) ->
    Reply={{running_clusters,State#state.running_clusters},{not_available_clusters,State#state.not_available_clusters}},
    {reply, Reply, State};

handle_call({running_clusters},_From,State) ->
    Reply=State#state.running_clusters,
    {reply, Reply, State};

handle_call({not_available_clusters},_From,State) ->
    Reply=State#state.not_available_clusters,
    {reply, Reply, State};

handle_call({status_cluster,_ClusterId},_From,State) ->
%    AllClusters=lists:append(State#state.running_clusters,State#state.not_available_clusters),
%    Reply=[{Status,XHostId,Ip,Port}||{Status,XHostId,Ip,Port}<-AllClusters,
	%		       HostId==XHostId],
    Reply=glurk,
    {reply, Reply, State};

%%------ Standard

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call({ping},_From,State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% -------------------------------------------------------------------
handle_cast({wanted_state,Interval}, State) ->
    spawn(fun()->wanted_state(Interval,State#state.running_clusters) end),    
    {noreply, State};
			     
handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
wanted_state(Interval,RunningClusters)->
    timer:sleep(2000),
    cluster:wanted_state(RunningClusters),
    timer:sleep(Interval),
    rpc:cast(node(),?MODULE,wanted_state,[Interval]).
 
%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
