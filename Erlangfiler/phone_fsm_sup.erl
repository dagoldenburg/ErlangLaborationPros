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

%Starts the phone supervisor
start_link()->
  supervisor:start_link({local,?MODULE},?MODULE,self()).

%Stops the supervisor
stop(SupPid) ->
  exit(SupPid,normal),
  ok.

%Adds a controller that permanently gets restarted if it dies, shutdown time is 5000 ms and the controller is a worker
%that is identified with its corresponding phonenumber.
add_controller(SupPid, PhoneNumber) ->
  Restart = permanent,
  Shutdown = 5000,
  Type = worker,
  ChildSpecifications = {PhoneNumber,{phone_fsm,start_link,[PhoneNumber]},Restart,Shutdown,Type,[phone_fsm]},
  supervisor:start_child(SupPid, ChildSpecifications).

%Removes the a childprocess identified with a corresponding phonenumber from the supervisor and stops the process
remove_controller(SupPid, PhoneNumber) ->
  {ok,Pid} = hlr:lookup_id(PhoneNumber),
  supervisor:delete_child(SupPid, PhoneNumber),
  phone_fsm:stop(Pid),
  ok.
%Supervisor flags, One for one restart flag(if a process dies only restart that process), max 5 restarts per second
init(_Args) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 5,
  MaxSecondsBetweenRestarts = 1,
  Flags = {RestartStrategy,MaxRestarts,MaxSecondsBetweenRestarts},
  {ok,{Flags,[]}}.