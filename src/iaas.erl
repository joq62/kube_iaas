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
		running_clusters,
		missing_clusters}).



%% --------------------------------------------------------------------
%% Definitions 
-define(WantedStateInterval,60*1000).

%% --------------------------------------------------------------------


-export([
	 cluster_strive_desired_state/1,
	 status_all_hosts/0,
	 running_hosts/0,
	 not_available_hosts/0,
	 status_host/1,
	 update_host_status/0
	]).

-export([
	 clusters_is_wanted_state/0,
	 wanted_state/1,
	 wanted_state/0,
	 create_cluster/1,
	 status_all_clusters/0,
	 running_clusters/0,
	 not_available_clusters/0,
	 status_cluster/1
	]).

-export([start/0,
	 stop/0,
	 ping/0

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
update_host_status()->
    gen_server:call(?MODULE, {update_host_status},infinity).
status_all_hosts()->
    gen_server:call(?MODULE, {status_all_hosts},infinity).
running_hosts()->
    gen_server:call(?MODULE, {running_hosts},infinity).
not_available_hosts()->
    gen_server:call(?MODULE, {not_available_hosts},infinity).
status_host(HostId)->
    gen_server:call(?MODULE, {status_host,HostId},infinity).

%%-----------------------------------------------------------------------

create_cluster(ClusterId)->
    gen_server:call(?MODULE, {create_cluster,ClusterId},infinity).

clusters_is_wanted_state()->
    gen_server:call(?MODULE, {clusters_is_wanted_state},infinity).
status_all_clusters()->
    gen_server:call(?MODULE, {status_all_clusters},infinity).
running_clusters()->
    gen_server:call(?MODULE, {running_clusters},infinity).
not_available_clusters()->
    gen_server:call(?MODULE, {not_available_clusters},infinity).
status_cluster(ClusterId)->
    gen_server:call(?MODULE, {status_cluster,ClusterId},infinity).


%%----------------------------------------------------------------------
cluster_strive_desired_state(ClusterStatus)->
    gen_server:cast(?MODULE, {cluster_strive_desired_state,ClusterStatus}).
wanted_state()->
    gen_server:cast(?MODULE, {wanted_state}).

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
    case rpc:call(node(),host,status_all_hosts,[],10*1000) of
	{ok,RH,NAH}->
	    RunningHosts=RH,
	    NotAvailableHosts=NAH;
	_->
	    RunningHosts=[],
	    NotAvailableHosts=[]
    end,
    rpc:call(node(),cluster,strive_desired_state,[],5*5000),
    ClusterStatus=rpc:call(node(),cluster,status_clusters,[],3*5000),
    case ClusterStatus of
	{{running,RunningClusters},{missing,MissingClusters}}->
	    ok;
	_->
	    RunningClusters=[],
	    MissingClusters=[]
    end,
    spawn(fun()->cl_strive_desired_state() end),    

    {ok, #state{running_hosts=RunningHosts,
		not_available_hosts=NotAvailableHosts,
		running_clusters=RunningClusters,
		missing_clusters=MissingClusters}}.
    
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
handle_call({update_host_status},_From,State) ->
    Reply= case rpc:call(node(),host,status_all_hosts,[],10*1000) of
	       {ok,RH,NAH}->
		   NewState=State#state{running_hosts=RH,
					not_available_hosts=NAH},
		   {ok,RH,NAH};
	       Err->
		   NewState=State,
		   {error,Err}
	   end,
        {reply, Reply, NewState};

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
    Reply={{running,State#state.running_clusters},{missing,State#state.missing_clusters}},
    {reply, Reply, State};

handle_call({running_clusters},_From,State) ->
    Reply=State#state.running_clusters,
    {reply, Reply, State};

handle_call({not_available_clusters},_From,State) ->
    Reply=State#state.missing_clusters,
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

handle_cast({cluster_strive_desired_state,ClusterStatus}, State) ->
 %   io:format("ClusterStatus ~p~n",[{?MODULE,?LINE,ClusterStatus}]),
    case ClusterStatus of
	{{running,Running},{missing,Missing}}->
	    NewState=State#state{running_clusters=Running,missing_clusters=Missing};
	_Err->
	    NewState=State
    end,
    spawn(fun()->cl_strive_desired_state() end),    
    {noreply, NewState};


handle_cast({wanted_state}, State) ->
    spawn(fun()->cluster:wanted_state(State#state.running_clusters) end),    
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
-define(ClusterStatusInterval,6*10000).
cl_strive_desired_state()->
    rpc:call(node(),cluster,strive_desired_state,[],5*5000),
    ClusterStatus=rpc:call(node(),cluster,status_clusters,[],3*5000),
  %  io:format("ClusterStatus ~p~n",[{?MODULE,?LINE,ClusterStatus}]),
    timer:sleep(?ClusterStatusInterval),
    rpc:cast(node(),?MODULE,cluster_strive_desired_state,[ClusterStatus]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
