%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: phone_fsm.erl
%%% @author jakdan@kth.se & dagol@kth.se
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(phone_fsm).
-behaviour(gen_statem).

%%%% EXPORTS %%%%%
-export([start_link/1,stop/1,connect/1,disconnect/1]).
-export([init/1,callback_mode/0]).
-export([busy/1,reject/1,accept/1,hangup/1,inbound/1]).
-export([action/2]).
%States
-export([disconnected/3,idle/3,connected/3,receiving/3,calling/3]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(PhoneNumber)->
	hlr:attach(PhoneNumber),
	gen_statem:start_link(?MODULE,[],[]).

stop(FsmPid)-> 
	hlr:detach(),
	exit(normal,self()),
	ok.
	
connect(FsmPid) ->
	gen_statem:cast(?MODULE, {connect,FsmPid}).


disconnect(FsmPid)->
    %When disconnected enter state: disconnected, where it cant be called
	gen_statem:cast(?MODULE, disconnect).
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([])->
	{ok, disconnected, self()}.
		
callback_mode() -> state_functions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%
busy(FsmPid) ->
  gen_statem:cast(?MODULE,busy),
  ok.
reject(FsmPid) ->
  gen_statem:cast(?MODULE,reject),
  ok.
accept(FsmPid) ->
  gen_statem:cast(?MODULE,accept),
  ok.
hangup(FsmPid) ->
  gen_statem:cast(?MODULE,hangup),
  ok.
inbound(FsmPid) ->
  gen_statem:cast(?MODULE,{inbound,hlr:lookup_phone(FsmPid)}),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(FsmPid,Action) ->
	gen_statem:cast(?MODULE,Action).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disconnect
%%%%%%%%%%%%%%%%%%%%%%%%%%%
disconnected(cast, {connect,FsmPid},FsmPid)->
	{next_state,idle,FsmPid};
disconnected(cast, _Other, LoopData)->
	{keep_state,LoopData}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Idle
%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast,{outbound,PhoneNumber},LoopData)->
	{next_state,calling,LoopData};
idle(cast,{inbound,PhoneNumber},LoopData)->
	{next_state,receiving,LoopData};
idle(cast, disconnect,LoopData)->
	{next_state,disconnected,LoopData};
idle(cast,_Other,LoopData)->
	{keep_state,LoopData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calling
%%%%%%%%%%%%%%%%%%%%%%%%%%%
calling(cast,hangup,LoopData)->
	{next_state,idle,LoopData};
calling(cast,accept,LoopData)->
	{next_state,connected,LoopData};
calling(cast, disconnect,LoopData)->
	{next_state,disconnected,LoopData};
calling(cast,_Other,LoopData)->
	{next_state,idle,LoopData}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Receiving
%%%%%%%%%%%%%%%%%%%%%%%%%%%

receiving(cast,reject,LoopData)->
	{next_state,idle,LoopData};
receiving(cast,accept,LoopData)->
	{next_state,connected,LoopData};
receiving(cast, disconnect,LoopData)->
	{next_state,disconnected,LoopData};
receiving(_,_Other,LoopData)->
	{keep_state,LoopData}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Connected
%%%%%%%%%%%%%%%%%%%%%%%%%%%
connected(cast,hangup,LoopData)->
	{next_state,idle,LoopData};
connected(cast, disconnect,LoopData)->
	{next_state,disconnected,LoopData};
connected(cast,_Other,LoopData)->
	{keep_state,LoopData}.
