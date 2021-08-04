-module(db_host_info).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,host_info).
-define(RECORD,host_info).
-record(host_info,{
		   alias,
		   host_id,
		   ip,
		   ssh_port,
		   uid,
		   pwd
		  }).

% Start Special 

% End Special 
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create(Alias,HostId,Ip,SshPort,UId,Pwd)->
    Record=#?RECORD{
		    alias=Alias,
		    host_id=HostId,
		    ip=Ip,
		    ssh_port=SshPort,
		    uid=UId,
		    pwd=Pwd
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Alias,HostId,Ip,SshPort,UId,Pwd}||{?RECORD,Alias,HostId,Ip,SshPort,UId,Pwd}<-Z].

read(Alias)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.alias==Alias])),
    [{XAlias,HostId,Ip,SshPort,UId,Pwd}||{?RECORD,XAlias,HostId,Ip,SshPort,UId,Pwd}<-Z].

delete(Alias,HostId,Ip,SshPort,UId,Pwd) ->
    F = fun() -> 
		ToBeRemoved=[X||X<-mnesia:read({?TABLE,HostId}),
				X#?RECORD.alias=:=Alias,
				X#?RECORD.host_id=:=HostId,
				X#?RECORD.ip=:=Ip,
				X#?RECORD.ssh_port=:=SshPort,
				X#?RECORD.uid=:=UId,
				X#?RECORD.pwd=:=Pwd
			    ],
		case ToBeRemoved of
		    []->
			mnesia:abort(no_to_remove);
		    ToBeRemoved ->
			[mnesia:delete_object(HostInfo)||HostInfo<-ToBeRemoved]
		end 
	end,
    mnesia:transaction(F).
 

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
