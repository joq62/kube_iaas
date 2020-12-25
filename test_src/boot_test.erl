%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Create1d : 10 dec 2012
%%% -------------------------------------------------------------------
-module(boot_test). 
    
%% --------------------------------------------------------------------
%% Include files

-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Definitions
-define(ClusterConfigDir,"cluster_config").
-define(ClusterConfigFileName,"cluster_info.hrl").
-define(GitUser,"joq62").
-define(GitPassWd,"20Qazxsw20").
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% External exports
-export([start/0]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ?debugMsg("Start setup"),
    ?assertEqual(ok,setup()),
    ?debugMsg("stop setup"),
    
    ?debugMsg("Start iaas"),
    ?assertEqual(ok,start_iaas()),
    ?debugMsg("stop setup"),

    ?assertEqual(ok,cleanup()),

    ?debugMsg("------>"++atom_to_list(?MODULE)++" ENDED SUCCESSFUL ---------"),
    ok.




%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_iaas()->
    ok=application:start(iaas),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
%    ServerId="server",
%    os:cmd("rm -rf server"),
 %   GitCmdLoadServer="git clone https://"++?GitUser++":"++?GitPassWd++"@github.com/"++?GitUser++"/server.git",
 %   os:cmd("git clone https://"++?GitUser++":"++?GitPassWd++"@github.com/"++?GitUser++"/server.git"),
    
%    ?assertEqual(ok,application:start(Server)),

setup()->
    % common
    os:cmd("rm -rf common"),
    os:cmd("git clone https://"++?GitUser++":"++?GitPassWd++"@github.com/"++?GitUser++"/common.git"),
    ?assertEqual(true,code:add_path("common/ebin")),
    ?assertEqual(ok,application:start(common)),
    
    % dbase
    os:cmd("rm -rf dbase"),
    os:cmd("git clone https://"++?GitUser++":"++?GitPassWd++"@github.com/"++?GitUser++"/dbase.git"),
  ?assertEqual(true,code:add_path("dbase/ebin")),
    ?assertEqual(ok,application:start(dbase)),
    % server
    os:cmd("rm -rf server"),
    os:cmd("git clone https://"++?GitUser++":"++?GitPassWd++"@github.com/"++?GitUser++"/server.git"),
  ?assertEqual(true,code:add_path("server/ebin")),
    ?assertEqual(ok,application:start(server)),
    server:preload_dbase(?ClusterConfigDir,?ClusterConfigFileName,?GitUser,?GitPassWd),

     % check if server has started dbase
    ?assertMatch([{"c2",_,_,"192.168.0.202",22,not_available},
		  {"c1",_,_,"192.168.0.201",22,not_available},
		  {"c0",_,_,"192.168.0.200",22,not_available}],
		 if_db:server_read_all()),
    ?assertMatch([{_,_}],
		 if_db:passwd_read_all()),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

cleanup()->

    ok.
