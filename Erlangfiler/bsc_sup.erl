%%%-------------------------------------------------------------------
%%% @author Dag
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Feb 2018 8:14 PM
%%%-------------------------------------------------------------------
-module(bsc_sup).
-author("Dag").

%% API
-export([]).
-export([start_link/0, stop/1, add_controller/1, init/1, remove_controller/1]).
%Starts a new empty phone controller supervisor that is linked to the calling process.
start_link()->
  supervisor:start_link({local,?MODULE},?MODULE,self()).

%Stop the phone controller supervisor and terminate all the existing phone controllers.
stop(SupPid) ->
  exit(SupPid,normal),
  ok.

%Start a new phone controller process for PhoneNumber and add it to the processes that the
%supervisor manages.
add_controller(PhoneNumber) ->
  phone_fsm_sup:add_controller(phone_fsm_sup,PhoneNumber),
  hlr:lookup_id(PhoneNumber).


%Remove the phone controller for PhoneNumber from the processes which the supervisor
%manages. Also terminate the controller process.
remove_controller(PhoneNumber) ->
  {ok,FsmPid} = hlr:lookup_id(PhoneNumber),
  phone_fsm_sup:remove_controller(phone_fsm_sup,PhoneNumber),
  {ok,FsmPid}.

init(_Args) ->
  io:format("supervisor starting~n"),
  RestartStrategy = one_for_one,
  MaxRestarts = 5,
  MaxSecondsBetweenRestarts = 1,
  Flags = {RestartStrategy,MaxRestarts,MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 5000,
  Type = worker,
  HlrSpecification = {hlr,{hlr,start_link,[]},Restart,Shutdown,Type,[hlr]},

  SupervisorType = supervisor,
  FsmSupervisorSpecification = {phone_fsm_sup,{phone_fsm_sup,start_link,[]},Restart,Shutdown,SupervisorType,[phone_fsm_sup]},

  {ok,{Flags,[HlrSpecification,FsmSupervisorSpecification]}}.