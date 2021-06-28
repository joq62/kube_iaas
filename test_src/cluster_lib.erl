%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(cluster_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(HostConfigPath,"https://github.com/joq62/host_config.git").
-define(GitHostConfigFile,"git clone https://github.com/joq62/host_config.git").
-define(Cookie,"abc").
-define(HostConfigDir,"host_config").
-define(HostConfigFile,"host_config/hosts.config").

-define(AppCatalogPath,"https://github.com/joq62/catalog.git").
-define(GitAppCatalog,"git clone https://github.com/joq62/catalog.git").
-define(AppCatalogDir,"catalog").
-define(AppCatalogFile,"catalog/application.catalog").

%% --------------------------------------------------------------------


%% External exports
-export([
	 load_config/3,
	 read_config/1,
	 status_hosts/1,
	 status_slaves/1,
	 start_masters/2,
	 start_slaves/4,
	 start_slaves/2

	]).


-export([
	 create/4,
	 start_app/5,
	 stop_app/4,
	 app_status/2

	]).

-define(WAIT_FOR_TABLES,5000).

%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
create(NumMasters,RunningHosts,Name,Cookie)->
   
    HostList=create_host_list(NumMasters,RunningHosts,[]),
    ok.



create_host_list(0,RunningHosts,HostsToUse)->
    HostsToUse;
create_host_list(N,[HostInfo|T],Acc) ->
    NewAcc=[HostInfo|Acc],
    NewRunningHosts=lists:append(T,[HostInfo]),
    create_host_list(N-1,NewRunningHosts,NewAcc).





%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start_slaves(HostIds,SlavesConfigFile)->
    F1=fun start_slave/2,
    F2=fun check_slave/3,
    Reply=case file:consult(SlavesConfigFile) of
	       {ok,SlaveInfoList}->
		  SlavesToStart=slaves_to_start(SlaveInfoList,HostIds,[]),
		%  io:format("SlavesToStart  ~p~n",[{SlavesToStart,?MODULE,?LINE}]),		  
		  mapreduce:start(F1,F2,[],SlavesToStart);
	      false->
		  {error,[noexist,SlavesConfigFile]}
	  end,
    Reply.

slaves_to_start([],_,SlavesToStart)->
    SlavesToStart;
slaves_to_start([{HostId,SlaveInfoList}|T],HostIds,Acc)->
    NewAcc=case lists:member(HostId,HostIds) of
	       true->
		   Master=list_to_atom("master"++"@"++HostId),
		   SlavesToStart=[{Master,HostId,SlaveName,ErlCmd}||{SlaveName,ErlCmd}<-SlaveInfoList],
		   lists:append(SlavesToStart,Acc);
	       false->
		   Acc
	   end,
    slaves_to_start(T,HostIds,NewAcc).


start_slaves(Master,HostId,SlaveNames,ErlCmd)->
    F1=fun start_slave/2,
    F2=fun check_slave/3,
    SlaveSToStart=[{Master,HostId,SlaveName,ErlCmd}||SlaveName<-SlaveNames],
    R1=mapreduce:start(F1,F2,[],SlaveSToStart),
    R1.
 
start_slave(Pid,{Master,HostId,SlaveName,ErlCmd})->
  %  io:format("SlaveName,Master  ~p~n",[{SlaveName,Master,?MODULE,?LINE}]),
    R=case rpc:call(Master,slave,stop,[list_to_atom(SlaveName++"@"++HostId)],2*5000) of
	  ok->
	      case rpc:call(Master,slave,start,[HostId,SlaveName,ErlCmd],2*5000) of
		  {ok,Slave}->
		      rpc:call(Master,os,cmd,["rm -rf "++SlaveName],5000),
		      case rpc:call(Master,file,make_dir,[SlaveName],5000) of
			  ok->
			      {ok,Slave};
			  Err->
			      {error,[Err,Master,HostId,SlaveName,?MODULE,?FUNCTION_NAME,?LINE]}
		      end;
		  Err->
		      {error,[Err,Master,HostId,SlaveName,?MODULE,?FUNCTION_NAME,?LINE]}
	      end;
	  Err ->
	      {error,[Err,Master,HostId,SlaveName,?MODULE,?FUNCTION_NAME,?LINE]}
      end,
 %   io:format("R  ~p~n",[{R,?MODULE,?LINE}]),
    Pid!{check_slave,{R}}.
check_slave(check_slave,Vals,[])->
  %  io:format("~p~n",[{?MODULE,?LINE,Key,Vals}]),
     check_slave(Vals,[]).


check_slave([],AllResult)->
    AllResult;
check_slave([{Result}|T],Acc)->
    check_slave(T,[Result|Acc]).



%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start_masters(HostIds,HostFile)->
    F1=fun start_master/2,
    F2=fun check_master/3,
    StatusHosts=status_hosts(HostFile),    
    {running,AllRunningHosts}=lists:keyfind(running,1,StatusHosts),
     HostsToStart=[[{host_id,HostId},{ip,Ip},{ssh_port,Port},{uid,Uid},{pwd,Pwd}]
		   ||[{host_id,HostId},
		      {ip,Ip},
		      {ssh_port,Port},
		      {uid,Uid},
		      {pwd,Pwd}]<-AllRunningHosts,
		     true==lists:member(HostId,HostIds)],
    R1=mapreduce:start(F1,F2,[],HostsToStart),
    R1.
    

start_master(Pid,[{host_id,HostId},{ip,Ip},{ssh_port,Port},{uid,Uid},{pwd,Pwd}])->
    X1=rpc:call(node(),my_ssh,ssh_send,[Ip,Port,Uid,Pwd,"rm -rf master",3000],7000),
 %   io:format("rm -rf master ~p~n",[{X1,?MODULE,?LINE}]),
    X2=rpc:call(node(),my_ssh,ssh_send,[Ip,Port,Uid,Pwd,"mkdir master",3000],7000),
 %   io:format("mkdir  master ~p~n",[{X2,?MODULE,?LINE}]),
    Stopped=stop_vm(HostId,"master"),
 %   io:format("Stopped ~p~n",[{Stopped,?MODULE,?LINE}]),
    ErlCmd="erl -detached -sname master -setcookie "++?Cookie,
						
    io:format("Ip,Port,Uid,Pwd ~p~n",[{Ip,Port,Uid,Pwd,?MODULE,?LINE}]),
    Result=rpc:call(node(),my_ssh,ssh_send,[Ip,Port,Uid,Pwd,ErlCmd,2*5000],3*5000),
    Pid!{check_master,{Result,HostId}}.
	      
check_master(check_master,Vals,[])->
  %  io:format("~p~n",[{?MODULE,?LINE,Key,Vals}]),
     check_master(Vals,[]).


check_master([],AllResult)->
    AllResult;
check_master([{Result,HostId}|T],Acc)->
    NewAcc=case Result of
	       ok->
		   case node_started(HostId,"master") of
		       true->
			   [{ok,HostId}|Acc];
		       false->
			   [{error,[host_not_started,HostId,?MODULE,?FUNCTION_NAME,?LINE]}|Acc]
		   end;
	        _->
		   [{Result,HostId}|Acc]
	   end,
    check_master(T,NewAcc).


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
status_slaves(SlavesConfigFile)->
    F1=fun ping_slave/2,
    F2=fun check_slave_ping/3,
    Reply=case file:consult(SlavesConfigFile) of
	       {ok,SlaveInfoList}->
		  SlavesToPing=slaves_to_check(SlaveInfoList,[]),
		%  io:format("SlavesToStart  ~p~n",[{SlavesToStart,?MODULE,?LINE}]),		  
		  mapreduce:start(F1,F2,[],SlavesToPing);
	      false->
		  {error,[noexist,SlavesConfigFile]}
	  end,
    Reply.

slaves_to_check([],SlavesToPing)->
    SlavesToPing;
slaves_to_check([{HostId,SlaveInfoList}|T],Acc)->
    SlavesToPing=[{SlaveName,HostId}||{SlaveName,_}<-SlaveInfoList],
    NewAcc=lists:append(SlavesToPing,Acc),
    slaves_to_check(T,NewAcc).

ping_slave(Pid,{SlaveName,HostId})->
   % io:format("SlaveName,HostId  ~p~n",[{SlaveName,HostId ,?MODULE,?LINE}]),
    Slave=list_to_atom(SlaveName++"@"++HostId),
    Result=net_adm:ping(Slave),
    Pid!{ping_slave,{Result,Slave,HostId}}.


check_slave_ping(ping_slave,Vals,[])->
  %  io:format("~p~n",[{?MODULE,?LINE,Key,Vals}]),
    check_slave_ping(Vals,[{running,[]},{missing,[]}]).
check_slave_ping([],[{running,Running},{missing,Missing}])->
    [{running,Running},{missing,Missing}];
check_slave_ping([{Result,Slave,HostId}|T],[{running,Acc1},{missing,Acc2}])->
    NewAcc=case Result of
	       pong->
		   [{running,[{Slave,HostId}|Acc1]},{missing,Acc2}];
	       pang->
		   [{running,Acc1},{missing,[{Slave,HostId}|Acc2]}]
	   end,
    check_slave_ping(T,NewAcc).


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
status_hosts(HostFile)->
    Reply=case filelib:is_file(HostFile) of
	      true->
		  {ok,HostInfoList}=file:consult(HostFile),
	%	  io:format("HostInfoList ~p~n",[{HostInfoList,?MODULE,?LINE}]),
		  host:status_hosts(HostInfoList);
	      false->
		  {error,[noexist,HostFile]}
	  end,
    Reply.

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
load_config(Dir,HostFile,GitCmd)->
    os:cmd("rm -rf "++Dir),
    os:cmd(GitCmd),
    Reply=case filelib:is_file(HostFile) of
	      true->
		  ok;
	      false->
		  {error,[noexist,HostFile]}
	  end,
    Reply.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
read_config(HostFile)->
    Reply=case filelib:is_file(HostFile) of
	      true->
		  file:consult(HostFile);
	      false->
		  {error,[noexist,HostFile]}
	  end,
    Reply.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
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
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
stop_app(ApplicationStr,Application,Dir,Vm)->
    rpc:call(Vm,os,cmd,["rm -rf "++Dir++"/"++ApplicationStr]),
    rpc:call(Vm,application,stop,[Application]),
    rpc:call(Vm,application,unload,[Application]).
    

start_app(ApplicationStr,Application,CloneCmd,Dir,Vm)->
    rpc:call(Vm,os,cmd,[CloneCmd++" "++Dir++"/"++ApplicationStr]),
    true=rpc:call(Vm,code,add_patha,[Dir++"/"++ApplicationStr++"/ebin"]),
    ok=rpc:call(Vm,application,start,[Application]),
    app_status(Vm,Application).

app_status(Vm,Application)->
    Status = case rpc:call(Vm,Application,ping,[]) of   
		 {pong,_,Application}->
		     running;
		 Err ->
		     {error,[Err]}
	     end,
    Status.

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
