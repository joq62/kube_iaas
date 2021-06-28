%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(oam_lib).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ClusterConfigPath,"https://github.com/joq62/cluster_config.git").
-define(ClusterConfigDir,"cluster_config").
-define(ClusterConfigFile,"cluster_config/cluster.config").

-define(HostConfigPath,"https://github.com/joq62/host_config.git").
-define(HostConfigDir,"host_config").
-define(HostConfigFile,"host_config/hosts.config").

-define(PodSpecsPath,"https://github.com/joq62/pod_specs.git").
-define(PodSpecsDir,"pod_specs").

%% --------------------------------------------------------------------

% New final ?

-export([
	 init_dbase/0

	]).

%% External exports
-export([
	 

	]).


-export([

	]).



%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
init_dbase()->
    %Start stop mnesia
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),

    % Init dbase
    ok=init_cluster_info(),
    ok=init_host_info(),
    ok=init_pod_specs(),
    ok.
    
  
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
init_cluster_info()->
    os:cmd("rm -rf "++?ClusterConfigDir),
    os:cmd("git clone "++?ClusterConfigPath),
    {ok,ClusterInfo}=file:consult(?ClusterConfigFile),
    ok=db_cluster_info:create_table(),
    ok=init_cluster_info(ClusterInfo,[]),
    ok.
init_cluster_info([],Result)->
    R=[R||R<-Result,
	  R/={atomic,ok}],
    case R of
	[]->
	    ok;
	R->
	    {error,[R]}
    end;
    
init_cluster_info([[{cluster_name,ClusterId},{controller_host,ControllerHost},{worker_hosts,NumWorkers,WorkerHosts},{cookie,Cookie}]|T],Acc)->
    ControllerNode=[],
    R=db_cluster_info:create(ClusterId,ControllerHost,NumWorkers,WorkerHosts,Cookie,ControllerNode),
    init_cluster_info(T,[R|Acc]).
    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
init_host_info()->
    os:cmd("rm -rf "++?HostConfigDir),
    os:cmd("git clone "++?HostConfigPath),
    {ok,HostInfo}=file:consult(?HostConfigFile),
    ok=db_host_info:create_table(),
    ok=init_host_info(HostInfo,[]),
    ok.
init_host_info([],Result)->
    R=[R||R<-Result,
	  R/={atomic,ok}],
    case R of
	[]->
	    ok;
	R->
	    {error,[R]}
    end;
    
init_host_info([[{host_id,HostId},{ip,Ip},{ssh_port,SshPort},{uid,UId},{pwd,Pwd}]|T],Acc)->
    R=db_host_info:create(HostId,Ip,SshPort,UId,Pwd),
    init_host_info(T,[R|Acc]).
    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
init_pod_specs()->
    os:cmd("rm -rf "++?PodSpecsDir),
    os:cmd("git clone "++?PodSpecsPath),
    ok=db_pod_spec:create_table(),
    {ok,FileNames}=file:list_dir(?PodSpecsDir),
    PodSpecFiles=[filename:join([?PodSpecsDir,FileName])||FileName<-FileNames,
							 filename:extension(FileName)==".pod_spec"],
    ok=init_pod_specs(PodSpecFiles,[]),
      
    ok.

init_pod_specs([],Result)->
    R=[R||R<-Result,
	  R/={atomic,ok}],
    case R of
	[]->
	    ok;
	R->
	    {error,[R]}
    end;
init_pod_specs([PodSpecFile|T],Acc)->
    {ok,Info}=file:consult(PodSpecFile),
    [{pod_id,PodId},{pod_vsn,PodVsn},{application,{AppId,AppVsn,AppGitPath}},{app_env,AppEnv},{app_hosts,AppHosts}]=Info,
    R=db_pod_spec:create(PodId,PodVsn,AppId,AppVsn,AppGitPath,AppEnv,AppHosts),
    init_pod_specs(T,[R|Acc]).
    
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
