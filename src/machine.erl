%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(machine).  



-export([status/1,
	 update_status/1,
	 read_status/1
	]).

%% ====================================================================
%% External functions
%% ============================ ========================================

%% -------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
read_status(all)->
    AllServers=if_db:server_read_all(),
    AllServersStatus=[{Status,HostId}||{HostId,_User,_PassWd,_IpAddr,_Port,Status}<-AllServers],
    Running=[HostId||{running,HostId}<-AllServersStatus],
    NotAvailable=[HostId||{not_available,HostId}<-AllServersStatus],
    [{running,Running},{not_available,NotAvailable}];

read_status(XHostId) ->
    AllServers=if_db:server_read_all(),
    [ServersStatus]=[Status||{HostId,_User,_PassWd,_IpAddr,_Port,Status}<-AllServers,
		     XHostId==HostId],
    ServersStatus.
					
    
%% -------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
update_status( [{running,Running},{not_available,NotAvailable}])->
    [if_db:server_update(HostId,running)||HostId<-Running],
    [if_db:server_update(HostId,not_available)||HostId<-NotAvailable],    
    ok.

%% -------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
status(all)->
    Status=status(),
    Running=[HostId||{running,HostId}<-Status],
    NotAvailable=[HostId||{not_available,HostId}<-Status],
    [{running,Running},{not_available,NotAvailable}];

status(HostId) ->
    Status=status(),
    Result=[XHostIdStatus||{XHostIdStatus,XHostId}<-Status,
	   HostId==XHostId],
    Result.

status()->
    F1=fun get_hostname/2,
    F2=fun check_host_status/3,
    
    AllServers=if_db:server_read_all(),
  %  io:format("AllServers = ~p~n",[{?MODULE,?LINE,AllServers}]),
    Status=mapreduce:start(F1,F2,[],AllServers),
  %  io:format("Status = ~p~n",[{?MODULE,?LINE,Status}]),
    Status.
        

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

get_hostname(Parent,{HostId,User,PassWd,IpAddr,Port,_Status})->    
  %  io:format("get_hostname= ~p~n",[{?MODULE,?LINE,HostId,User,PassWd,IpAddr,Port}]),
    Msg="hostname",
    Result=my_ssh:ssh_send(IpAddr,Port,User,PassWd,Msg, 5*1000),
  %  io:format("Result, HostId= ~p~n",[{?MODULE,?LINE,Result,HostId}]),
    Parent!{machine_status,{HostId,Result}}.

check_host_status(machine_status,Vals,_)->
    check_host_status(Vals,[]).

check_host_status([],Status)->
    Status;
check_host_status([{HostId,[HostId]}|T],Acc)->
    NewAcc=[{running,HostId}|Acc],
    check_host_status(T,NewAcc);
check_host_status([{HostId,{error,_Err}}|T],Acc) ->
    check_host_status(T,[{not_available,HostId}|Acc]);
check_host_status([X|T],Acc) ->
    check_host_status(T,[{x,X}|Acc]).

    

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
