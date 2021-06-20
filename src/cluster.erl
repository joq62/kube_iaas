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



%% --------------------------------------------------------------------
-export([create/1]).


%% ====================================================================
%% External functions
%% ====================================================================

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
	      NodeName=ClusterId++"_"++"kubelet",
	      erlang:set_cookie(node(),list_to_atom(Cookie)),
	      ListToReduce=[[Info,NodeName,Cookie]||Info<-AllHostInfo],
	      RunningKubeletNodes=[{XNode,XHostId}||{ok,XNode,XHostId,_,_}<-mapreduce:start(F1,F2,[],ListToReduce)]
	      
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
    timer:sleep(1000),
 %   io:format("UniqueNodeName = ~p~n",[UniqueNodeName]),
    ErlCmd="erl -detached "++"-sname "++UniqueNodeName++" "++"-setcookie "++Cookie,						
   
 %   io:format("Ip,Port,Uid,Pwd ~p~n",[{Ip,SshPort,UId,Pwd,?MODULE,?LINE}]),
    Result=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,ErlCmd,2*5000],3*5000),
 %   io:format("Result ~p~n",[{Result,?MODULE,?LINE}]),
    Node=list_to_atom(UniqueNodeName++"@"++HostId),
    Pid!{check_node,{Result,Node,HostId,Ip,SshPort}}.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
check_node(check_node,Vals,[])->
    io:format("Vals ~p~n",[{?MODULE,?LINE,Vals}]),
     check_node(Vals,[]).

check_node([],Result)->
    Result;
check_node([{Result,Node,HostId,Ip,SshPort}|T],Acc)->
    NewAcc=case Result of
	       ok->
		   case node_started(Node) of
		       true->
			   [{ok,Node,HostId,Ip,SshPort}|Acc];
		       false->
			   [{error,[host_not_started,Node,HostId,Ip,SshPort,?MODULE,?FUNCTION_NAME,?LINE]}|Acc]
		   end;
	        _->
		   [{Result,Node,HostId,Ip,SshPort}|Acc]
	   end,
    check_node(T,NewAcc).
node_started(Node)->
       check_started(50,Node,10,false).
    
check_started(_N,Vm,_SleepTime,true)->
   true;
check_started(0,Vm,_SleepTime,Result)->
    Result;
check_started(N,Vm,SleepTime,_Result)->
%    io:format("N,Vm ~p~n",[{N,Vm,SleepTime,?MODULE,?LINE}]),
    NewResult=case net_adm:ping(Vm) of
		  pong->
		     true;
		  _Err->
		      timer:sleep(SleepTime),
		      false
	      end,
    check_started(N-1,Vm,SleepTime,NewResult).
