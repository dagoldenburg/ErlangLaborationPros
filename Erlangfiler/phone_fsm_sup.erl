%%%-------------------------------------------------------------------
%%% @author Dag
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Feb 2018 6:55 PM
%%%-------------------------------------------------------------------
-module(phone_fsm_sup).
-author("Dag").

-behaviour(supervisor).

%% API
-export([start_link/0, stop/1, add_controller/2, remove_controller/2, init/1]).
%Starts a new empty phone controller supervisor that is linked to the calling process.
start_link()->
  supervisor:start_link({local,?MODULE},?MODULE,self()).

%Stop the phone controller supervisor and terminate all the existing phone controllers.
stop(SupPid) ->
  exit(SupPid,normal),
  ok.

%Start a new phone controller process for PhoneNumber and add it to the processes that the
%supervisor manages.
add_controller(SupPid, PhoneNumber) ->
  Restart = permanent,
  Shutdown = 5000,
  Type = worker,
  ChildSpecifications = {PhoneNumber,{phone_fsm,start_link,[PhoneNumber]},Restart,Shutdown,Type,[phone_fsm]},
  supervisor:start_child(SupPid, ChildSpecifications).

%Remove the phone controller for PhoneNumber from the processes which the supervisor
%manages. Also terminate the controller process.
remove_controller(SupPid, PhoneNumber) ->
  {ok,Pid} = hlr:lookup_id(PhoneNumber),
  supervisor:delete_child(SupPid, PhoneNumber),
  phone_fsm:stop(Pid),
  ok.

init(_Args) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 5,
  MaxSecondsBetweenRestarts = 1,
  Flags = {RestartStrategy,MaxRestarts,MaxSecondsBetweenRestarts},
  {ok,{Flags,[]}}.