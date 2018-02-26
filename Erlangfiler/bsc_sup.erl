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
%Starts the supervisor
start_link()->
  supervisor:start_link({local,?MODULE},?MODULE,self()).

%Stops the supervisor(and all children)
stop(SupPid) ->
  exit(SupPid,normal),
  ok.

%Adds a child process under the supervisors controlled, which will be identified with a corresponding phonenumber
add_controller(PhoneNumber) ->
  phone_fsm_sup:add_controller(phone_fsm_sup,PhoneNumber),
  hlr:lookup_id(PhoneNumber).


%Remove a child from supervisor management, identified by phonenumber
remove_controller(PhoneNumber) ->
  {ok,FsmPid} = hlr:lookup_id(PhoneNumber),
  phone_fsm_sup:remove_controller(phone_fsm_sup,PhoneNumber),
  {ok,FsmPid}.


%Supervisor flags, one for all(both are vital and connected if one dies just reset the whole system), max 5 restarts per
%second. Also starts the HLR and supervisor for all phone controllers, they both restart no matter what.
init(_Args) ->
  io:format("supervisor starting~n"),
  RestartStrategy = one_for_all,
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