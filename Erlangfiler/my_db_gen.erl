%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: my_db_gen.erl
%%% @author jakdan@kth.se
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(my_db_gen).
-behaviour(gen_server).

-export([start/0,start_link/0,stop/0]).
-export([match/1,write/2,read/1,delete/1]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2]).




%% @doc
start()->
	gen_server:start({local,?MODULE}, ?MODULE, [], []).

start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [],[]).
	
	
%% @doc Ends the database instance
stop() ->
	gen_server:cast(?MODULE,stop).

	
	

%% @doc Writes an element with a key to the database
write(Key,Element) ->
	gen_server:cast(?MODULE, {write,self(),Key,Element}).

%% @doc Reads the element of a given key from the database
read(Key) ->
	gen_server:call(?MODULE, {read,self(),Key}).

%% @doc Deletes an entry in the database with the given Key
delete(Key) ->
	gen_server:cast(?MODULE,{delete,self(),Key}).

%% @doc Takes an element and returns a list of all keys with matching elements
match(Element) ->
	gen_server:call(?MODULE, {match,self(),Element}).

	
init([]) ->
	Db = dbtree:new(),
	{ok,Db}.
	
terminate(_, Db) ->
	dbtree:destroy(Db).


handle_call({read,_,Key},_From,Db) ->
	{reply,dbtree:read(Key,Db),Db};
handle_call({match,_,Element},_From,Db) ->
	{reply,dbtree:match(Element,Db),Db}.
	
handle_cast({write,_,Key,Element},Db) ->
	Db1 = dbtree:write(Key,Element,Db),
	{noreply,Db1};
handle_cast({delete,_,Key},Db) ->
	Db1 = dbtree:delete(Key,Db),
	{noreply,Db1}.


	