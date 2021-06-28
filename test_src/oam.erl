%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Create Controller per cluster, the Controller ceates the cluster 
%%% Check health per cluster
%%% Delete a cluster
%%% Add + remove hosts per cluster  
%%% Install Cluster
%%% Install cluster
%%% Data-{HostId,Ip,SshPort,Uid,Pwd}
%%% available_hosts()-> [{HostId,Ip,SshPort,Uid,Pwd},..]
%%% install_leader_host({HostId,Ip,SshPort,Uid,Pwd})->ok|{error,Err}
%%% cluster_status()->[{running,WorkingNodes},{not_running,NotRunningNodes}]

%%% Created : 
%%% -------------------------------------------------------------------
-module(oam).  
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
 
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state, {}).



%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------





% OaM related
% Admin
-export([
	 status_all_clusters/0
	]).


-export([
	 status_hosts/0,
	 cluster_info/0,
	 host_info/0,
	 catalog_info/0
	]).

% Operate
-export([
	 create_cluster/1,
	 create_cluster/4,
	 
	 status_cluster/1
	]).

-export([
	

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

%% Asynchrounus Signals


%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


%%  Admin 
status_hosts()->
    gen_server:call(?MODULE, {status_hosts},infinity).
cluster_info()->
    gen_server:call(?MODULE, {cluster_info},infinity).   
host_info()->
    gen_server:call(?MODULE, {host_info},infinity).  
catalog_info()->
    gen_server:call(?MODULE, {catalog_info},infinity).  

%%---------------------------------------------------------------

create_cluster(ClusterName,NumControllers,Hosts,Cookie)->
    gen_server:call(?MODULE, {create_cluster,ClusterName,Cookie,Hosts,NumControllers},infinity).
create_cluster(ClusterId)->
    gen_server:call(?MODULE, {create_cluster,ClusterId},infinity).
delete_cluster(ClusterId)->
    gen_server:call(?MODULE, {delete,ClusterId},infinity).
status_all_clusters()->    
    gen_server:call(?MODULE, {status_all_clusters},infinity).
status_cluster(ClusterId)->
    gen_server:call(?MODULE, {status_cluster,ClusterId},infinity).
    



%%---------------------------------------------------------------

ping()-> 
    gen_server:call(?MODULE, {ping},infinity).

%%-----------------------------------------------------------------------

%%----------------------------------------------------------------------


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------


init([]) ->
    % Stop and restart mnesia
    ok=oam_lib:init_dbase(),
   
    
    
    {ok, #state{}}.
    
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


handle_call({cluster_info},_From,State) ->
    Reply=etcd:cluster_info(),
    {reply, Reply, State};

handle_call({host_info},_From,State) ->
    Reply=etcd:host_info(),
    {reply, Reply, State};

handle_call({catalog_info},_From,State) ->
    Reply=etcd:catalog_info(),
    {reply, Reply, State};


handle_call({status_hosts},_From,State) ->
    Reply=host_controller:status_hosts(),
    {reply, Reply, State};

handle_call({create_cluster,ClusterName},_From,State) ->
    Reply=rpc:call(node(),oam_cluster,create,[ClusterName]),
    {reply, Reply, State};

handle_call({ping},_From,State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

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

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
