%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: hlr.erl
%%% @author jakdan@kth.se & dagol@kth.se
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hlr).
-behaviour(gen_server).

-export([start_link/0,stop/0]).
-export([attach/1,detach/0,lookup_id/1,lookup_phone/1]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2]).



start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [],[]).
	
	
%% @doc Ends the database instance
stop() ->
	gen_server:cast(?MODULE,stop).

	
	
attach(PhoneNumber) ->
	gen_server:cast(?MODULE, {write,self(), PhoneNumber}).

detach() ->
	gen_server:cast(?MODULE, {delete,self()}).

lookup_id(PhoneNumber) ->
	gen_server:call(?MODULE, {match,self(),PhoneNumber}).

lookup_phone(Pid) ->
	gen_server:call(?MODULE, {read,self(),Pid}).
	
init([]) ->
	Db = dbtree:new(),
	{ok,Db}.
	
terminate(_, Db) ->
	dbtree:destroy(Db).


handle_call({read,_,Key},_From,Db) ->
	{reply,dbtree:read(Key,Db),Db};
handle_call({match,_,Element},_From,Db) ->
	{reply,dbtree:match(Element,Db),Db}.
	
handle_cast({write,Key,Element},Db) ->
	Db1 = dbtree:write(Key,Element,Db),
	{noreply,Db1};
handle_cast({delete,Key},Db) ->
	Db1 = dbtree:delete(Key,Db),
	{noreply,Db1}.


	