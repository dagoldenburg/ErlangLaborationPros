%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: hlr.erl
%%% @author jakdan@kth.se & dagol@kth.se
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hlr).
-behaviour(gen_server).

-export([start_link/0, stop/0, handle_info/2, code_change/3]).
-export([attach/1,detach/0,lookup_id/1,lookup_phone/1]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2]).

start_link()->
	gen_server:start_link({global,hlren}, hlr, [],[]).

	
%% @doc Ends the database instance
stop() ->
	gen_server:stop({global,hlren}).

attach(PhoneNumber) ->
	gen_server:cast({global,hlren}, {write,self(), PhoneNumber}).

detach() ->
	gen_server:cast({global,hlren}, {delete,self()}).

lookup_id(PhoneNumber) ->
	gen_server:call({global,hlren}, {match,PhoneNumber}).

lookup_phone(Pid) ->
	gen_server:call({global,hlren}, {read,Pid}).
	
init([]) ->
	{ok,db:new()}.
	
terminate(_, Db) ->
	db:destroy(Db),
	ok.
handle_call({read,Key},_From,Db) ->
	{reply,db:read(Key,Db),Db};
handle_call({match,Element},_From,Db) ->
	Result = dbtree:flatten(db:match(Element,Db)),
	case Result of
		[] -> {reply,{error,invalid},Db};
		[H|_T] -> {reply,{ok,H},Db}
	end.
	

handle_cast(stop,Hlr) ->
	
	{stop,normal,Hlr};	
handle_cast({write,Key,Element},Db) ->
	{noreply,db:write(Key,Element,Db)};

handle_cast({delete,Key},Db) ->
	Db1 = db:delete(Key,Db),
	{noreply,Db1}.

handle_info(_Info, _State) ->
	erlang:error(not_implemented).

code_change(_OldVsn, _State, _Extra) ->
	erlang:error(not_implemented).