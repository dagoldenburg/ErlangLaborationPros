%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: hlr.erl
%%% @author jakdan@kth.se & dagol@kth.se
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hlr).
-behaviour(gen_server).

-export([start_link/0, stop/0, handle_info/2, code_change/3]).
-export([attach/1,detach/0,lookup_id/1,lookup_phone/1]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2]).

%Starts a global HLR process
start_link()->
	gen_server:start_link({global,hlren}, hlr, [],[]).

	
%Stops the HLR process
stop() ->
	gen_server:stop({global,hlren}).

%Attach a Phone Number with a Phone controller pid.
attach(PhoneNumber) ->
	gen_server:cast({global,hlren}, {write,self(), PhoneNumber}).

%Detach a phone number from a Phone controller pid.
detach() ->
	gen_server:cast({global,hlren}, {delete,self()}).

%Get the Phone controller pid attached to a phone number.
lookup_id(PhoneNumber) ->
	gen_server:call({global,hlren}, {match,PhoneNumber}).

%Get the phone number attached to a Phone controller pid
lookup_phone(Pid) ->
	gen_server:call({global,hlren}, {read,Pid}).

%Initiates a new database instance
init([]) ->
	{ok,db:new()}.
	
%Called when HLR process stops, destroys the database instance.
terminate(_, Db) ->
	db:destroy(Db),
	ok.
	
%Synchronous database functions
handle_call({read,Key},_From,Db) -> %Returns value of specified Key from database.
	{reply,db:read(Key,Db),Db};
handle_call({match,Element},_From,Db) -> %Returns list of Keys with matching Element.
	Result = dbtree:flatten(db:match(Element,Db)),
	case Result of
		[] -> {reply,{error,invalid},Db};
		[H|_T] -> {reply,{ok,H},Db}
	end.
	
%Asynchronous database functions
handle_cast(stop,Hlr) -> %Terminates the database instance
	{stop,normal,Hlr};	
handle_cast({write,Key,Element},Db) -> %Adds a new entry to the database. Overwrites already existing Keys.
	{noreply,db:write(Key,Element,Db)};
handle_cast({delete,Key},Db) -> %Deletes an entry from the database.
	Db1 = db:delete(Key,Db),
	{noreply,Db1}.

	
%Not yet implemented Gen Server functions
handle_info(_Info, _State) ->
	erlang:error(not_implemented).
code_change(_OldVsn, _State, _Extra) ->
	erlang:error(not_implemented).