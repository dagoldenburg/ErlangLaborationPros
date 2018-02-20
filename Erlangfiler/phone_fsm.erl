%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File: phone_fsm.erl
%%% @author jakdan@kth.se & dagol@kth.se
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(phone_fsm).
-behaviour(gen_statem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(PhoneNumber)->
hlr:attach(PhoneNumber)
	gen_statem:start_link({local,?MODULE}, ?MODULE,[],[]).


outbound(PhoneNumber) ->
	gen_statem:cast(?MODULE, {outbound,PhoneNumber}).
	
inbound(Pid)->
gen_statem:cast()
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
init([])->
	{ok, idle, null}.
		
callback_mode() -> state_functions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Idle
%%%%%%%%%%%%%%%%%%%%%%%%%%%
idle(cast,{outbound,PhoneNumber},LoopData)->
	{next_state,calling,LoopData};
idle(cast,{inbound,PhoneNumber},LoopData)->
	{next_state,receiving,LoopData};
idle(cast,_Other,LoopData)->
	{keep_state,LoopData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calling
%%%%%%%%%%%%%%%%%%%%%%%%%%%
calling(cast,hangup,LoopData)->
	{next_state,idle,LoopData};
calling(cast,accept,LoopData)->
	{next_state,connected,LoopData};
calling(cast,_Other,LoopData)->
	{next_state,idle,LoopData}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Receiving
%%%%%%%%%%%%%%%%%%%%%%%%%%%

receiving(cast,reject,LoopData)->
	{next_state,idle,LoopData};
receiving(cast,accept,LoopData)->
	{next_state,connected,LoopData};
receiving(_,_Other,LoopData)->
	{keep_state,LoopData}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Connected
%%%%%%%%%%%%%%%%%%%%%%%%%%%
connected(cast,hangup,LoopData)->
	{next_state,idle,LoopData};
connected(cast,_Other,LoopData)->
	{keep_state,LoopData}.
