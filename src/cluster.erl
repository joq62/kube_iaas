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

%%---------------------------------------------------------------------
%% Records & defintions
%%---------------------------------------------------------------------
%missing clusters
%running clusters

-define(KubeletNodeName(ClusterId),ClusterId++"_"++"kubelet").
-define(KubeletNode(ClusterId,HostId),list_to_atom(ClusterId++"_"++"kubelet"++"_"++HostId++"@"++HostId)).
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
    {{running,R},{missing,M}}=cluster:status_clusters(),
%    io:format("Status_clusters() = ~p~n",[{?MODULE,?LINE,{{running,R},{missing,M}}}]),    
    [{cluster:create(ClusterId),ClusterId}||{ClusterId,_}<-M].

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
status_clusters()->
    check(db_cluster_info:read_all(),[],[]).
    
check([],Running,Missing)->
    {{running,Running},{missing,Missing}};
check([{ClusterId,[ControllerHost],_,_,Cookie,_}|T],Running,Missing) ->
    Node=?KubeletNode(ClusterId,ControllerHost),
    erlang:set_cookie(node(),list_to_atom(Cookie)),
    case net_adm:ping(Node) of
	pong->
	    NewRunning=[{ClusterId,Node}|Running],
	    NewMissing=Missing;
	pang ->
	    NewMissing=[{ClusterId,Node}|Missing],
	    NewRunning=Running
    end,
    check(T,NewRunning,NewMissing).
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
		[{ClusterId,[ControllerHost],NumWorkers,WorkerHosts,Cookie,ControllerNode}]=ClusterInfo,
		KubeletNode=?KubeletNode(ClusterId,ControllerHost),
%		io:format("KubeletNode  ~p~n",[net_adm:ping(KubeletNode)]),
		rpc:call(KubeletNode,init,stop,[])	      
	end,
    R.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create(ClusterId)->
    F1=fun start_node/2,
    F2=fun check_node/3,
    R=case db_cluster_info:read(ClusterId) of
	  []->
	      {error,[eexists,ClusterId]};
	  ClusterInfo->
	      [{ClusterId,ControllerHost,NumWorkers,WorkerHosts,Cookie,ControllerNode}]=ClusterInfo,
	      H1=[XHostId||XHostId<-WorkerHosts,
			  false==lists:member(XHostId,ControllerHost)],
	      AllHosts=lists:append(ControllerHost,H1),
	      AllHostInfo=lists:append([db_host_info:read(HostId)||HostId<-AllHosts]),
	      NodeName=?KubeletNodeName(ClusterId),  %ClusterId++"_"++"kubelet",
	      erlang:set_cookie(node(),list_to_atom(Cookie)),
	      ListToReduce=[[Info,NodeName,Cookie]||Info<-AllHostInfo],
	      RunningKubeletNodes=[{XNode,XHostId}||{ok,XNode,XHostId,_,_}<-mapreduce:start(F1,F2,[],ListToReduce)],
	      {ok,ClusterId,RunningKubeletNodes}
	      
      end,
    R.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_nodes(Hosts,NodeName,Cookie)->
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_node(Pid,[{HostId,Ip,SshPort,UId,Pwd},NodeName,Cookie])->
    UniqueNodeName=NodeName++"_"++HostId,
    Node=list_to_atom(UniqueNodeName++"@"++HostId),
    rpc:call(Node,init,stop,[]),
    erlang:set_cookie(node(),list_to_atom(Cookie)),
    timer:sleep(1000),
 %   io:format("UniqueNodeName = ~p~n",[UniqueNodeName]),
    ErlCmd="erl -detached "++"-sname "++UniqueNodeName++" "++"-setcookie "++Cookie,						
   
 %   io:format("Ip,Port,Uid,Pwd ~p~n",[{Ip,SshPort,UId,Pwd,?MODULE,?LINE}]),
    Result=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,ErlCmd,2*5000],3*5000),
 %   case rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,ErlCmd,2*5000],3*5000) of
	
    Node=list_to_atom(UniqueNodeName++"@"++HostId),
 %   io:format("Result ~p~n",[{?MODULE,?LINE,Result,Node,HostId,Ip,SshPort}]),
    Pid!{check_node,{Result,Node,HostId,Ip,SshPort}}.


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

check_node([{{badrpc,timeout},Node,HostId,Ip,SshPort}|T],Acc)->
    check_node(T,Acc); 
check_node([{{error,_Err},Node,HostId,Ip,SshPort}|T],Acc)->
    check_node(T,Acc); 
  
check_node([{Result,Node,HostId,Ip,SshPort}|T],Acc)->
%    io:format(" ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE, Result,HostId,Ip}]),
    NewAcc=case Result of
	       ok->
		   case node_started(Node) of
		       true->
		%	   io:format("ok ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Node,HostId,Ip,SshPort}]),
			   [{ok,Node,HostId,Ip,SshPort}|Acc];
		       false->
		%	   io:format("error,host_not_started,~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Node,HostId,Ip,SshPort}]),
			   [{error,[host_not_started,Node,HostId,Ip,SshPort,?MODULE,?FUNCTION_NAME,?LINE]}|Acc]
		   end;
	        Err->
		 %  io:format("Err ,~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,Err,Node,HostId,Ip,SshPort}]),
		   [{Result,Node,HostId,Ip,SshPort}|Acc]
	   end,
    check_node(T,NewAcc).
node_started(Node)->
       check_started(10,Node,10,false).
    
check_started(_N,Vm,_SleepTime,true)->
   true;
check_started(0,Vm,_SleepTime,Result)->
    Result;
check_started(N,Vm,SleepTime,_Result)->
    io:format("N,Vm ~p~n",[{N,Vm,SleepTime,?MODULE,?LINE}]),
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
