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
	gen_statem:start_link(?MODULE,[PhoneNumber],[]).

stop(FsmPid)-> 
	gen_statem:cast(FsmPid,disconnect),
	ok.
	
connect(FsmPid) ->
	gen_statem:cast(FsmPid, {connect,self()}).


disconnect(FsmPid)->
    %When disconnected enter state: disconnected, where it cant be called
	gen_statem:cast(FsmPid, disconnect).
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([PhoneNumber])->
	hlr:attach(PhoneNumber),
	{ok, disconnected, self()}.
		
callback_mode() -> state_functions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%
busy(FsmPid) ->
  gen_statem:cast(FsmPid,busy),
  ok.
reject(FsmPid) ->
  gen_statem:cast(FsmPid,reject),
  ok.
accept(FsmPid) ->
  gen_statem:cast(FsmPid,accept),
  ok.
hangup(FsmPid) ->
  gen_statem:cast(FsmPid,disconnect),
  ok.
inbound(FsmPid) ->
  gen_statem:cast(FsmPid,{inbound,self()}),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(FsmPid,Action) ->
	gen_statem:cast(FsmPid,Action).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disconnect
%%%%%%%%%%%%%%%%%%%%%%%%%%%
disconnected(cast, {connect,PhonePid},_LoopData)->
	io:fwrite("fwrite: PHONE_FSM disconnected Connect~n", []),
	{next_state,idle,PhonePid};
disconnected(cast, disconnect,_LoopData)->
	hlr:detach(),
	{keep_state,null};
disconnected(cast, _Other, LoopData)->
	io:fwrite("fwrite: PHONE_FSM disconnected _Other~n", []),
	{keep_state,LoopData}.


	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Idle
%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast,{outbound,PhoneNumber},SelfPhonePid)->
	io:fwrite("fwrite: PHONE_FSM idle outbound~n", []),
	Result = hlr:lookup_id(PhoneNumber), %%%%%%%%%LOOKUP FSM PID
	case Result of
		{error,invalid}->
			{keep_state,SelfPhonePid};
		{ok,Pid}->
			phone_fsm:inbound(Pid),
			{next_state,calling,{SelfPhonePid,Pid}}
	end;
idle(cast,{inbound,OtherFsmPid},SelfPhonePid)->
	io:fwrite("fwrite: PHONE_FSM idle inbound~n", []),
	phone:reply(SelfPhonePid,{inbound,something}),
	{next_state,receiving,{SelfPhonePid,OtherFsmPid}};
idle(cast, disconnect,LoopData)->
	io:fwrite("fwrite: PHONE_FSM idle disconnect~n", []),
	hlr:detach(),
	{next_state,disconnected,LoopData};
idle(cast,_Other,LoopData)->
	io:fwrite("fwrite: PHONE_FSM idle OTHER~n", []),
	{keep_state,LoopData}.

	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calling
%%%%%%%%%%%%%%%%%%%%%%%%%%%
calling(cast,reject,{SelfPhonePid,_OtherFsmPid})->
	io:fwrite("fwrite: PHONE_FSM calling reject~n", []),
	phone:reply(SelfPhonePid,reject), %%%%%%%%%%%%%%%%PHONE PID???
	{next_state,idle,SelfPhonePid};
calling(cast,accept,{SelfPhonePid,OtherFsmPid})->
	io:fwrite("fwrite: PHONE_FSM calling accept~n", []),
	phone:reply(SelfPhonePid,accept),
	{next_state,connected,{SelfPhonePid,OtherFsmPid}};
calling(cast, disconnect,{_SelfPhonePid,_OtherFsmPid})->
	io:fwrite("fwrite: PHONE_FSM calling disconnect~n", []),
	hlr:detach(),
	{next_state,disconnected,null};
calling(cast,busy,{SelfPhonePid,_OtherFsmPid})->
	io:fwrite("fwrite: PHONE_FSM calling busy~n", []),
	phone:reply(SelfPhonePid,busy),
	{next_state,idle,SelfPhonePid};	
calling(cast, {inbound,OtherFsmPid},LoopData)->
	io:fwrite("fwrite: PHONE_FSM calling inbound~n", []),
	phone_fsm:busy(OtherFsmPid),
	{keep_state,LoopData};
calling(cast,_Other,LoopData)->
	io:fwrite("fwrite: PHONE_FSM calling other~n", []),
	{keep_state,LoopData}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Receiving
%%%%%%%%%%%%%%%%%%%%%%%%%%%
receiving(cast,reject,{SelfPhonePid,OtherFsmPid})->
	phone_fsm:reject(OtherFsmPid),
	io:fwrite("fwrite: PHONE_FSM receiving reject~n", []),
	{next_state,idle,SelfPhonePid};
receiving(cast,accept,{SelfPhonePid,OtherFsmPid})->
	phone_fsm:accept(OtherFsmPid),
	io:fwrite("fwrite: PHONE_FSM receiving accept~n", []),
	{next_state,connected,{SelfPhonePid,OtherFsmPid}};
receiving(cast, disconnect,_LoopData)->
	io:fwrite("fwrite: PHONE_FSM receiving disconnect~n", []),
	hlr:detach(),
	{next_state,disconnected,null};
receiving(cast, {inbound,OtherFsmPid},LoopData)->
	io:fwrite("fwrite: PHONE_FSM receiving inbound~n", []),
	phone_fsm:busy(OtherFsmPid),
	{keep_state,LoopData};
receiving(_,_Other,LoopData)->
	io:fwrite("fwrite: PHONE_FSM receiving OTHER~n", []),
	{keep_state,LoopData}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Connected
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connected(cast,hangup,{SelfPhonePid,_OtherFsmPid})->
	io:fwrite("fwrite: PHONE_FSM connected hangup~n", []),
	{next_state,idle,SelfPhonePid};
connected(cast, disconnect,{_SelfPhonePid,_OtherFsmPid})->
	io:fwrite("fwrite: PHONE_FSM connected disconnect~n", []),
	hlr:detach(),
	{next_state,disconnected,null};
connected(cast, {inbound,OtherFsmPid},LoopData)->
	io:fwrite("fwrite: PHONE_FSM connected inbound~n", []),
	phone_fsm:busy(OtherFsmPid),
	{keep_state,LoopData};
connected(cast,_Other,LoopData)->
	io:fwrite("fwrite: PHONE_FSM connected OTHER~n", []),
	{keep_state,LoopData}.
