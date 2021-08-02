%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(cluster).    
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("kube_logger.hrl").
%%---------------------------------------------------------------------
%% Records & defintions
%%---------------------------------------------------------------------
%missing clusters
%running clusters

-define(KubeletNodeName(ClusterId),ClusterId++"_"++"kubelet").
-define(KubeletNode(ClusterId,Alias,HostId),list_to_atom(ClusterId++"_"++"kubelet"++"_"++Alias++"@"++HostId)).
%% --------------------------------------------------------------------
-export([
       	 strive_desired_state/0,
	 status_clusters/0,
	 create/1,
	 delete/1
	]).


%% ====================================================================
%% External functions
%% ====================================================================  

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
strive_desired_state()->
   %   io:format("~p~n",[{?FUNCTION_NAME,?MODULE,?LINE}]),
    {{running,_R},{missing,M}}=cluster:status_clusters(),
    [{cluster:create(ClusterId),ClusterId}||{ClusterId,_}<-M].

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
status_clusters()->
    {ok,ClusterId}=application:get_env(cluster_id),
    R=case db_cluster_info:read(atom_to_list(ClusterId)) of
	  []->
	      ?PrintLog(alert,"ClusterID eexists",[ClusterId,?FUNCTION_NAME,?MODULE,?LINE]),
	      {error,["ClusterID eexists"]};
	  ClusterInfo->
	      check(ClusterInfo,[],[])
      end,
   % io:format("R ~p~n",[{R,?FUNCTION_NAME,?MODULE,?LINE}]),
    R.
    
check([],Running,Missing)->
    {{running,Running},{missing,Missing}};

check([{ClusterId,ControllerAlias,_,WorkerAlias,Cookie,_}|T],Running,Missing) ->
    H1=[XAlias||XAlias<-WorkerAlias,
		false==lists:member(XAlias,ControllerAlias)],
    AllAlias=lists:append(ControllerAlias,H1),
    AllHostInfo=[db_host_info:read(Alias)||Alias<-AllAlias],
    NodesToCheck=[?KubeletNode(ClusterId,Alias,HostId)||[{Alias,HostId,_Ip,_SshPort,_UId,_Pwd}]<-AllHostInfo],
%    erlang:set_cookie(node(),list_to_atom(Cookie)),   
   {R1,M1}=do_ping(NodesToCheck,ClusterId,[],[]),
    case M1 of
	[]->
	    NewRunning=[{ClusterId,R1}|Running],
	    NewMissing=Missing;
	_ ->
	    NewMissing=[{ClusterId,M1}|Missing],
	    NewRunning=Running
    end,   
    check(T,NewRunning,NewMissing).

do_ping([],_ClusterId,Running,Missing)->
    {Running,Missing};
do_ping([Node|T],ClusterId,Running,Missing)->
    case net_adm:ping(Node) of
	pong->
	    NewRunning=[Node|Running],
	    NewMissing=Missing;
	pang ->
	    NewMissing=[Node|Missing],
	    NewRunning=Running
    end,    
    do_ping(T,ClusterId,NewRunning,NewMissing).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
delete(ClusterId)->
      R=case db_cluster_info:read(ClusterId) of
	  []->
	      {error,[eexists,ClusterId]};
	  ClusterInfo->
		[{ClusterId,ControllerAlias,_NumWorkers,WorkerAlias,Cookie,_ControllerNode}]=ClusterInfo,
	%	erlang:set_cookie(node(),list_to_atom(Cookie)),
	%	?PrintLog(debug,"set_cookie",[Cookie,?FUNCTION_NAME,?MODULE,?LINE]),
		 H1=[XAlias||XAlias<-WorkerAlias,
			     false==lists:member(XAlias,ControllerAlias)],
		AllAlias=lists:append(ControllerAlias,H1),
		AllHostInfo=[db_host_info:read(Alias)||Alias<-AllAlias],
		NodesToKill=[?KubeletNode(ClusterId,Alias,HostId)||[{Alias,HostId,_Ip,_SshPort,_UId,_Pwd}]<-AllHostInfo],
%		io:format("KubeletNode  ~p~n",[net_adm:ping(KubeletNode)]),
		?PrintLog(log,"Stopped Cluster Nodes",[ClusterId,NodesToKill]),
		[{Node,ClusterId,delete_cluster(Node,ClusterId)}||Node<-NodesToKill]	      
	end,
    R.
delete_cluster(Node,ClusterId)->
    rpc:call(Node,os,cmd,["rm -rf "++ClusterId],5*1000),
    rpc:call(Node,init,stop,[]),
    {stopped,Node,ClusterId}.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
%create(ClusterId)->
 %   create(ClusterId,controller).
			 
create(ClusterId)->
    F1=fun start_node/2,
    F2=fun check_node/3,
    R=case db_cluster_info:read(ClusterId) of
	  []->
	      ?PrintLog(ticket,"ClusterId eexists",[{error,[eexists,ClusterId]},?FUNCTION_NAME,?MODULE,?LINE]),
	      {error,[eexists,ClusterId]};
	  ClusterInfo->
	      [{ClusterId,ControllerAlias,_NumWorkers,WorkerAlias,Cookie,_ControllerNode}]=ClusterInfo,
	      NodeName=?KubeletNodeName(ClusterId),  %ClusterId++"_"++"kubelet",
	    %  erlang:set_cookie(node(),list_to_atom(Cookie)),	
	      H1=[XAlias||XAlias<-WorkerAlias,
			  false==lists:member(XAlias,ControllerAlias)],
	      AllAlias=lists:append(ControllerAlias,H1),
	      R1=create_list_to_reduce(AllAlias,NodeName,ClusterId,Cookie),	     
	      case R1 of
		  {error,ErrAlias}->
		      ?PrintLog(ticket,"reduce list",[{error,ErrAlias},?FUNCTION_NAME,?MODULE,?LINE]),
		      {error,ErrAlias};
		  {ok,ListToReduce}->
		      StartResult=mapreduce:start(F1,F2,[],ListToReduce),
		    %  io:format("StartResult ~p~n",[{?MODULE,?LINE,StartResult}]), 
		      {ClusterId,StartResult}
	      
	      end
      end,
    R.

create_list_to_reduce(AllAlias,NodeName,ClusterId,Cookie)->
    create_list_to_reduce(AllAlias,NodeName,ClusterId,Cookie,[]).
create_list_to_reduce([],_NodeName,_ClusterId,_Cookie,Acc)->
    case [{error,Reason}||{error,Reason}<-Acc] of
	[]->
	%    ReduceInfo=lists:append([XInfo||{ok,XInfo}<-Acc]),
	    ReduceInfo=[XInfo||{ok,XInfo}<-Acc],
	    {ok,ReduceInfo};
	ErrList->
	    {error,ErrList}
    end;
create_list_to_reduce([Alias|T],NodeName,ClusterId,Cookie,Acc)->
 %   ?PrintLog(debug,"Cookie ",[Cookie,NodeName,?FUNCTION_NAME,?MODULE,?LINE]),
    Info=case db_host_info:read(Alias) of
	     []->
		 {error,[eexists,Alias]};
	     [AliasInfo]->
		 {ok,[AliasInfo,NodeName,ClusterId,Cookie]}
	 end,
    create_list_to_reduce(T,NodeName,ClusterId,Cookie,[Info|Acc]).    

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_node(Pid,[{Alias,HostId,Ip,SshPort,UId,Pwd},NodeName,ClusterId,Cookie])->
    {Result,Node}=start_node([{Alias,HostId,Ip,SshPort,UId,Pwd},NodeName,ClusterId,Cookie]),
    Pid!{check_node,{Result,Node,ClusterId,HostId,Ip,SshPort}}.
    
start_node([{Alias,HostId,Ip,SshPort,UId,Pwd},NodeName,ClusterId,Cookie])->
 %   io:format("start_node ~p~n",[{?MODULE,?LINE,Alias,HostId,Ip,SshPort,NodeName,Cookie}]),
    RM_cluster="rm -rf "++ClusterId,
    RM_Result=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,RM_cluster,2*5000],3*5000),
    ?PrintLog(log,"ssh ",[RM_cluster,RM_Result,Alias,?FUNCTION_NAME,?MODULE,?LINE]),
    MKDIR_cluster="mkdir "++ClusterId,
    MKDIR_result=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,MKDIR_cluster,2*5000],3*5000),
    ?PrintLog(log,"ssh ",[MKDIR_cluster,MKDIR_result,Alias,?FUNCTION_NAME,?MODULE,?LINE]),
    UniqueNodeName=NodeName++"_"++Alias,
 %   erlang:set_cookie(node(),list_to_atom(Cookie)),
    timer:sleep(1000),
    Node=list_to_atom(UniqueNodeName++"@"++HostId),
    Result= case rpc:call(node(),net_adm,ping,[Node],1*1000) of
		pong->
		    ?PrintLog(ticket,"already started ",[Node,ClusterId,Alias,?FUNCTION_NAME,?MODULE,?LINE]),
		    ok;
		_->
		    NodeStop=rpc:call(Node,init,stop,[]),
		    ?PrintLog(log,"init stop ",[NodeStop,Node,?FUNCTION_NAME,?MODULE,?LINE]),
%		    ?PrintLog(debug,"Cookie ",[Cookie,Node,?FUNCTION_NAME,?MODULE,?LINE]),
		 %   ?PrintLog(debug,"Node Cookie ",[rpc:call(node(),erlang,get_cookie,[]),node(),?FUNCTION_NAME,?MODULE,?LINE]),
		    ErlCmd="erl -noshell -noinput "++"-sname "++UniqueNodeName++" "++"-setcookie "++Cookie,
		    SshCmd="nohup "++ErlCmd++" &",
		    ErlcCmdResult=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,SshCmd,2*5000],3*5000),
		    ?PrintLog(log,"ssh ",[ErlcCmdResult,SshCmd,Node,?FUNCTION_NAME,?MODULE,?LINE]),
		    ErlcCmdResult
	    end,
   % io:format("Result ~p~n",[{?MODULE,?LINE,Result,Node,Alias,Ip,SshPort}]),
    {Result,Node}.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
check_node(check_node,Vals,[])->
  %  io:format("Vals ~p~n",[{?MODULE,?LINE,Vals}]),
     check_node(Vals,[]).

check_node([],Result)->
    Result;

check_node([{{badrpc,timeout},Node,ClusterId,Alias,Ip,SshPort}|T],Acc)->
    ?PrintLog(ticket,"Failed to start node",[Node,Alias,{badrpc,timeout},?FUNCTION_NAME,?MODULE,?LINE]),
    check_node(T,[{error,[{badrpc,timeout},Node,ClusterId,Alias,Ip,SshPort]}|Acc]); 
check_node([{{error,Reason},Node,ClusterId,Alias,Ip,SshPort}|T],Acc)->
     ?PrintLog(ticket,"Failed to start node",[Node,Alias,{error,Reason},?FUNCTION_NAME,?MODULE,?LINE]),
     check_node(T,[{error,[Reason,Node,Alias,Ip,SshPort]}|Acc]);
  
check_node([{Result,Node,ClusterId,Alias,Ip,SshPort}|T],Acc)->
  %  ?PrintLog(debug,"Result",[Result,Alias,Node,ClusterId]),
 %   ?PrintLog(debug,"Cookie after created ",[rpc:call(Node,erlang,get_cookie,[]),Node,?FUNCTION_NAME,?MODULE,?LINE]),
    NewAcc=case Result of
	       ok->
		   case node_started(Node) of
		       true->
			   % laod_start kubelet
			   [PodSpec]=db_pod_spec:read("kubelet"),
			   {ok,MonitorNode}=application:get_env(monitor_node),
			   case pod:load_start(Node,ClusterId,MonitorNode,PodSpec) of
			       ok->
%				   ?PrintLog(debug,"Cookie after kubelte ",[rpc:call(Node,erlang,get_cookie,[]),Node,?FUNCTION_NAME,?MODULE,?LINE]),
				   ?PrintLog(log,"Started succesful",[Alias,Node,Ip,SshPort]),
				   [{ok,Node,Alias,Ip,SshPort}|Acc];
			       ErrStartKubelet->
				   ?PrintLog(ticket,"Failed to start kubelet",[ErrStartKubelet,Alias,Node,Ip,SshPort]),
				   [{error,[kubelet_not_started,Node,Alias,Ip,SshPort,ErrStartKubelet,?MODULE,?FUNCTION_NAME,?LINE]}|Acc]
			   end;
		       false->
			   ?PrintLog(ticket,"Failed to connect to node",[Node,Alias,?FUNCTION_NAME,?MODULE,?LINE]),
			   [{error,[host_not_started,Node,Alias,Ip,SshPort,?MODULE,?FUNCTION_NAME,?LINE]}|Acc]
		   end;
	        Err->
		   ?PrintLog(ticket,"error",[Err,Node,Alias,?FUNCTION_NAME,?MODULE,?LINE]),
		   [{Result,Node,Alias,Ip,SshPort}|Acc]
	   end,
    check_node(T,NewAcc).
node_started(Node)->
       check_started(50,Node,10,false).
    
check_started(_N,_Vm,_SleepTime,true)->
   true;
check_started(0,_Vm,_SleepTime,Result)->
    Result;
check_started(N,Vm,SleepTime,_Result)->
 %   io:format("net_Adm ~p~n",[net_adm:ping(Vm)]),
    NewResult= case net_adm:ping(Vm) of
	%case rpc:call(node(),net_adm,ping,[Vm],1000) of
		  pong->
		     true;
		  pang->
		      timer:sleep(SleepTime),
		      false;
		  {badrpc,_}->
		       timer:sleep(SleepTime),
		      false
	      end,
    check_started(N-1,Vm,SleepTime,NewResult).
