-module(db_cluster_info).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,cluster_info).
-define(RECORD,cluster_info).
-record(cluster_info,{
		      cluster_id,
		      controller_host,
		      num_worker_hosts,
		      worker_hosts,
		      cookie,
		      controller_node
		  }).

% Start Special 

% End Special 
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create(ClusterId,ControllerHost,NumWorkers,WorkerHosts,Cookie,ControllerNode)->
    Record=#?RECORD{
		    cluster_id=ClusterId,
		    controller_host=ControllerHost,
		    num_worker_hosts=NumWorkers,
		    worker_hosts=WorkerHosts,
		    cookie=Cookie,
		    controller_node=ControllerNode
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{ClusterId,ControllerHost,NumWorkers,WorkerHosts,Cookie,ControllerNode}||
	{?RECORD,ClusterId,ControllerHost,NumWorkers,WorkerHosts,Cookie,ControllerNode}<-Z].

read(ClusterId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_id==ClusterId])),
    [{XClusterId,ControllerHost,NumWorkers,WorkerHosts,Cookie,ControllerNode}||
	       {?RECORD,XClusterId,ControllerHost,NumWorkers,WorkerHosts,Cookie,ControllerNode}<-Z].

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
