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
    {{running,_R},{missing,M}}=cluster:status_clusters(),
%    io:format("Status_clusters() = ~p~n",[{?MODULE,?LINE,{{running,R},{missing,M}}}]),    
    [{cluster:create(ClusterId),ClusterId}||ClusterId<-M].

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
status_clusters()->
    check(db_cluster_info:read_all(),[],[]).
    
check([],Running,Missing)->
    {{running,Running},{missing,Missing}};

check([{ClusterId,ControllerAlias,_,WorkerAlias,Cookie,_}|T],Running,Missing) ->
   % io:format("ControllerAlias, WorkerAlias ~p~n",[{?FUNCTION_NAME,?MODULE,?LINE,ControllerAlias, WorkerAlias}]),
    H1=[XAlias||XAlias<-WorkerAlias,
		false==lists:member(XAlias,ControllerAlias)],
    AllAlias=lists:append(ControllerAlias,H1),
  %  io:format("AllAlias ~p~n",[{?FUNCTION_NAME,?MODULE,?LINE,AllAlias}]),
    AllHostInfo=[db_host_info:read(Alias)||Alias<-AllAlias],
 %   io:format("AllHostInfo ~p~n",[{?FUNCTION_NAME,?MODULE,?LINE,AllHostInfo}]),
    NodesToCheck=[?KubeletNode(ClusterId,Alias,HostId)||[{Alias,HostId,_Ip,_SshPort,_UId,_Pwd}]<-AllHostInfo],
    erlang:set_cookie(node(),list_to_atom(Cookie)),   
 %   io:format("ClusterId,NodesToCheck~p~n",[{?FUNCTION_NAME,?MODULE,?LINE,ClusterId,NodesToCheck}]),
    {R1,M1}=do_ping(NodesToCheck,ClusterId,[],[]),
 %   io:format(" {R1,M1} ~p~n",[{?FUNCTION_NAME,?MODULE,?LINE,R1,M1}]),
    case M1 of
	[]->
	    NewRunning=[ClusterId|Running],
	    NewMissing=Missing;
	_ ->
	    NewMissing=[ClusterId|Missing],
	    NewRunning=Running
    end,   
  %  io:format("NewRunning, NewMissing~p~n",[{?FUNCTION_NAME,?MODULE,?LINE,NewRunning,NewMissing}]),
    check(T,NewRunning,NewMissing).

do_ping([],ClusterId,Running,Missing)->
    {Running,Missing};
do_ping([Node|T],ClusterId,Running,Missing)->
   % io:format("Node,ClusterId,Running,Missing ~p~n",[{?FUNCTION_NAME,?MODULE,?LINE,Node,ClusterId,Running,Missing}]),
%    io:format("Node,ping ~p~n",[{?FUNCTION_NAME,?MODULE,?LINE,Node,net_adm:ping(Node)}]),
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
		erlang:set_cookie(node(),list_to_atom(Cookie)),
		 H1=[XAlias||XAlias<-WorkerAlias,
			     false==lists:member(XAlias,ControllerAlias)],
		AllAlias=lists:append(ControllerAlias,H1),
		AllHostInfo=[db_host_info:read(Alias)||Alias<-AllAlias],
		NodesToKill=[?KubeletNode(ClusterId,Alias,HostId)||[{Alias,HostId,_Ip,_SshPort,_UId,_Pwd}]<-AllHostInfo],
%		io:format("KubeletNode  ~p~n",[net_adm:ping(KubeletNode)]),
		?PrintLog(log,"Stopped Cluster Nodes",[ClusterId,NodesToKill]),
		[{rpc:call(Node,init,stop,[]),Node}||Node<-NodesToKill]	      
	end,
    R.

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
	      {error,[eexists,ClusterId]};
	  ClusterInfo->
	      [{ClusterId,ControllerAlias,_NumWorkers,WorkerAlias,Cookie,_ControllerNode}]=ClusterInfo,
	      NodeName=?KubeletNodeName(ClusterId),  %ClusterId++"_"++"kubelet",
	      erlang:set_cookie(node(),list_to_atom(Cookie)),	
	      H1=[XAlias||XAlias<-WorkerAlias,
			  false==lists:member(XAlias,ControllerAlias)],
	      AllAlias=lists:append(ControllerAlias,H1),
	      R1=create_list_to_reduce(AllAlias,NodeName,Cookie),	     
	      case R1 of
		  {error,ErrAlias}->
		        {error,ErrAlias};
		  {ok,ListToReduce}->
		      StartResult=mapreduce:start(F1,F2,[],ListToReduce),
		    %  io:format("StartResult ~p~n",[{?MODULE,?LINE,StartResult}]), 
		      {ClusterId,StartResult}
	      
	    %  RunningKubeletNodes=[{XNode,XHostId}||{ok,XNode,XHostId,_,_}<-mapreduce:start(F1,F2,[],ListToReduce)],
	     % {ClusterId,mapreduce:start(F1,F2,[],ListToReduce)}
	      
	    %  {ok,ClusterId,RunningKubeletNodes}
	      
	      end
      end,
    R.

create_list_to_reduce(AllAlias,NodeName,Cookie)->
    create_list_to_reduce(AllAlias,NodeName,Cookie,[]).
create_list_to_reduce([],_NodeName,_Cookie,Acc)->
    case [{error,Reason}||{error,Reason}<-Acc] of
	[]->
	%    ReduceInfo=lists:append([XInfo||{ok,XInfo}<-Acc]),
	    ReduceInfo=[XInfo||{ok,XInfo}<-Acc],
	    {ok,ReduceInfo};
	ErrList->
	    {error,ErrList}
    end;
create_list_to_reduce([Alias|T],NodeName,Cookie,Acc)->
    Info=case db_host_info:read(Alias) of
	     []->
		 {error,[eexists,Alias]};
	     [AliasInfo]->
		 {ok,[AliasInfo,NodeName,Cookie]}
	 end,
    create_list_to_reduce(T,NodeName,Cookie,[Info|Acc]).    
  %  io:format("AllAlias ~p~n",[{?MODULE,?LINE,AllAlias}]),
    %glurk= 'hur sklla man kolla att all input är korrect' ,
    %AllAliasInfo=lists:append([db_host_info:read(Alias)||Alias<-AllAlias]),
  %  io:format("AllAliasInfo ~p~n",[{?MODULE,?LINE,AllAliasInfo}]),	
  %  ListToReduce=[[Info,NodeName,Cookie]||Info<-AllAliasInfo],
  %  io:format("ListToReduce ~p~n",[{?MODULE,?LINE,ListToReduce}]),


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
start_node(Pid,[{Alias,HostId,Ip,SshPort,UId,Pwd},NodeName,Cookie])->
    {Result,Node}=start_node([{Alias,HostId,Ip,SshPort,UId,Pwd},NodeName,Cookie]),
    Pid!{check_node,{Result,Node,HostId,Ip,SshPort}}.
    
start_node([{Alias,HostId,Ip,SshPort,UId,Pwd},NodeName,Cookie])->
 %   io:format("start_node ~p~n",[{?MODULE,?LINE,Alias,HostId,Ip,SshPort,NodeName,Cookie}]),
    UniqueNodeName=NodeName++"_"++Alias,
    erlang:set_cookie(node(),list_to_atom(Cookie)),
    timer:sleep(1000),
    Node=list_to_atom(UniqueNodeName++"@"++HostId),
    Result= case rpc:call(node(),net_adm,ping,[Node],1*1000) of
		pong->
		    io:format("already started  ~p~n",[{Node,Ip,SshPort,UId,Pwd,?MODULE,?LINE}]),
		    ok;
		_->
		    rpc:call(Node,init,stop,[]),
		    ErlCmd="erl -detached "++"-sname "++UniqueNodeName++" "++"-setcookie "++Cookie,						   rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,ErlCmd,2*5000],3*5000)
	    end,
   % io:format("Result ~p~n",[{?MODULE,?LINE,Result,Node,Alias,Ip,SshPort}]),
    {Result,Node}.

%start_node(Pid,[{HostId,Ip,SshPort,UId,Pwd},NodeName,Cookie])->
%    UniqueNodeName=NodeName++"_"++HostId,
%    erlang:set_cookie(node(),list_to_atom(Cookie)),
%    timer:sleep(1000),
%    Node=list_to_atom(UniqueNodeName++"@"++HostId),
%    Result= case rpc:call(node(),net_adm,ping,[Node],1*1000) of
%		pong->
%		     io:format("already started  ~p~n",[{Ip,SshPort,UId,Pwd,?MODULE,?LINE}]),
%		    ok;
%		_->
%		    rpc:call(Node,init,stop,[]),
%		    ErlCmd="erl -detached "++"-sname "++UniqueNodeName++" "++"-setcookie "++Cookie,				%		   rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,ErlCmd,2*5000],3*5000)
%	    end,
 %   Pid!{check_node,{Result,Node,HostId,Ip,SshPort}}.

		    %   io:format("UniqueNodeName = ~p~n",[UniqueNodeName]),
 %   case rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,ErlCmd,2*5000],3*5000) of
 %   io:format("Ip,Port,Uid,Pwd ~p~n",[{Ip,SshPort,UId,Pwd,?MODULE,?LINE}]),	
 %   io:format("Result ~p~n",[{?MODULE,?LINE,Result,Node,HostId,Ip,SshPort}]),
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

check_node([{{badrpc,timeout},Node,Alias,Ip,SshPort}|T],Acc)->
    ?PrintLog(ticket,"Failed to start",[ Alias,Node,Ip,SshPort,{badrpc,timeout}]),
    check_node(T,[{error,[{badrpc,timeout},Node,Alias,Ip,SshPort]}|Acc]); 
check_node([{{error,Reason},Node,Alias,Ip,SshPort}|T],Acc)->
    ?PrintLog(ticket,"Failed to start",[ Alias,Node,Ip,SshPort,{error,Reason}]),
     check_node(T,[{error,[Reason,Node,Alias,Ip,SshPort]}|Acc]);
  
check_node([{Result,Node,Alias,Ip,SshPort}|T],Acc)->
%    io:format(" ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE, Result,HostId,Ip}]),
    NewAcc=case Result of
	       ok->
		   case node_started(Node) of
		       true->
			%   io:format("Node started  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Node,HostId,Ip,SshPort}]),
			   ?PrintLog(log,"Started succesful",[Alias,Node,Ip,SshPort]),
			  % io:format("~s: ~w, ~s, Data: ~s, ~w, ~s, ~w ~n",
			%	     [misc_fun:date_time(),log,"Started succesful ",Alias,Node,Ip,SshPort]),
			   [{ok,Node,Alias,Ip,SshPort}|Acc];
		       false->
		%	   io:format("error,host_not_started,~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Node,HostId,Ip,SshPort}]),
			   ?PrintLog(ticket,"Failed to start",[Alias,Node,Ip,SshPort]),
			%   io:format("Ticket Failed to started  HostId: ~s, Node: ~w, IP: ~s, Port: ~w ~n",[Alias,Node,Ip,SshPort]),
			   [{error,[host_not_started,Node,Alias,Ip,SshPort,?MODULE,?FUNCTION_NAME,?LINE]}|Acc]
		   end;
	        _Err->
		 %  io:format("Err ,~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Err,Node,HostId,Ip,SshPort}]),
		   ?PrintLog(ticket,"Failed to start",[Alias,Node,Ip,SshPort]),
		   [{Result,Node,Alias,Ip,SshPort}|Acc]
	   end,
  %  io:format(" NewAcc ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,NewAcc}]),
    check_node(T,NewAcc).
node_started(Node)->
       check_started(10,Node,10,false).
    
check_started(_N,_Vm,_SleepTime,true)->
   true;
check_started(0,_Vm,_SleepTime,Result)->
    Result;
check_started(N,Vm,SleepTime,_Result)->
  %  io:format("N,Vm ~p~n",[{N,Vm,SleepTime,?MODULE,?LINE}]),
    NewResult= case net_adm:ping(Vm) of
	%case rpc:call(node(),net_adm,ping,[Vm],1000) of
		  pong->
		     true;
		  pang->
		      timer:sleep(SleepTime),
		      false;
		  {badrpc,_}->
		      false
	      end,
    check_started(N-1,Vm,SleepTime,NewResult).
