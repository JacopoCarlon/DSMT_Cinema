%%%-------------------------------------------------------------------
%%% @author nickrick3
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(main_supervisor).
-author("nickrick3").

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  %% supervisor:start_link registers the process globally as 'main_supervisor'
  {State, SupervisorPid} = supervisor:start_link({global, ?MODULE}, ?MODULE, []),
  io:format("[SUPERVISOR] Supervisor state is ~p, pid is ~p~n", [State, SupervisorPid]),
  SupervisorPid.

%% callback function called by 'supervisor' module
init(_Args) ->
  io:format("[SUPERVISOR] Init function ~n"),

  %% Supervisor Flags
  %% Strategy one_for_one: if one child process crashes, only that child process is restarted
  %% Supervisor crashes with all its children if more than 'intensity' children
  %% crash in a 'period' (expressed in seconds)
  SupFlags = #{
    strategy => one_for_one,
    intensity => 1,
    period => 5
  },

  MainServerChild = #{
    id => main_server,
    start => {main_server, start_server, []},
    restart => permanent
  },
  
  ShowMonitorChild = #{
    id => show_monitor,
    start => {show_monitor, start_show_monitor, []},
    restart => permanent
  },

  Children = [MainServerChild, ShowMonitorChild],

  %% Returns specification to 'supervisor' module
  {ok, {SupFlags, Children}}.
