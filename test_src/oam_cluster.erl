%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(oam_cluster).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records & defintions
%%---------------------------------------------------------------------



%% --------------------------------------------------------------------
-export([start_node_hosts/3,
	 create/1]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start_node_hosts(HostIds,NodeName,Cookie)->
    F1=fun start_node/2,
    F2=fun check_node/3,
    true=erlang:set_cookie(node(),list_to_atom(Cookie)),
    timer:sleep(100),
    AllHosts=etcd:host_info_all(),
    HostsToStart=[[{HostId,Ip,SshPort,UId,Pwd},NodeName,Cookie]
		  ||{HostId,Ip,SshPort,UId,Pwd}<-AllHosts,
		    lists:member(HostId,HostIds)],
    R1=mapreduce:start(F1,F2,[],HostsToStart),
    R1.
    

start_node(Pid,[{HostId,Ip,SshPort,UId,Pwd},NodeName,Cookie])->
    _Stopped=stop_vm(HostId,NodeName),
    ErlCmd="erl -detached -sname "++NodeName++" -setcookie "++Cookie,
						
%    io:format("Ip,Port,Uid,Pwd ~p~n",[{Ip,Port,Uid,Pwd,?MODULE,?LINE}]),
    Result=rpc:call(node(),my_ssh,ssh_send,[Ip,SshPort,UId,Pwd,ErlCmd,2*5000],3*5000),
    io:format("HostId ~p~n",[{HostId,Result,?MODULE,?LINE}]),
    Pid!{check_node,{Result,HostId,NodeName}}.
	      
check_node(check_node,Vals,[])->
  %  io:format("~p~n",[{?MODULE,?LINE,Key,Vals}]),
     check_node(Vals,[]).
check_node([],AllResult)->
    AllResult;
check_node([{Result,HostId,NodeName}|T],Acc)->
    NewAcc=case Result of
	       ok->
		   case node_started(HostId,NodeName) of
		       true->
			   [{ok,HostId,list_to_atom(NodeName++"@"++HostId)}|Acc];
		       false->
			   [{error,[host_not_started,HostId,?MODULE,?FUNCTION_NAME,?LINE]}|Acc]
		   end;
	        _->
		   [{Result,HostId,list_to_atom(NodeName++"@"++HostId)}|Acc]
	   end,
    check_node(T,NewAcc).


% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

node_started(HostId,NodeName)->
    Vm=list_to_atom(NodeName++"@"++HostId),
    check_started(50,Vm,10,false).
    
check_started(_N,_Vm,_SleepTime,true)->
    true;
check_started(0,_Vm,_SleepTime,Result)->
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

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
stop_vm(HostId,VmId)->
    Vm=list_to_atom(VmId++"@"++HostId),
    stop_vm(Vm).

stop_vm(Vm)->
    rpc:cast(Vm,init,stop,[]),
    vm_stopped(Vm).

vm_stopped(Vm)->
    check_stopped(50,Vm,100,false).
    
check_stopped(_N,_Vm,_SleepTime,true)->
    ok;
check_stopped(0,_Vm,_SleepTime,Result)->
    Result;
check_stopped(N,Vm,SleepTime,_Result)->
    NewResult=case net_adm:ping(Vm) of
		  pang->
		     true;
		  _Err->
		      timer:sleep(SleepTime),
		      false
	      end,
    check_stopped(N-1,Vm,SleepTime,NewResult).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
create(ClusterName)->
    R=case etcd:cluster_info(ClusterName) of
	  []->
	      {error,[eexists,ClusterName]};
	  [{CluserName,NumControllers,Hosts,Cookie,Nodes,Deployed}]->
	      {CluserName,NumControllers,Hosts,Cookie,Nodes,Deployed}

						% Change cookie to Cookie
	 
	       % start a vm with controller_ClausterName@HostId
	      

      end,
    R.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

    
