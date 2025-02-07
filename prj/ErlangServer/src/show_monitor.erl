%%%-------------------------------------------------------------------
%%% @author nickrick3
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. jan 2025 12:14
%%%-------------------------------------------------------------------
-module(show_monitor).
-author("nickrick3").

-export([start_show_monitor/0]).


start_show_monitor() ->
    io:format("[SHOW MONITOR] Show Monitor started with pid ~p~n", [self()]),
    register(show_monitor, self()),
    show_monitor_loop(maps:new()).


show_monitor_loop(MonitoredProcesses) ->
    receive
        {add_show_monitor, Pid, ShowId, ShowName, ShowDate, CinemaId, CinemaName, CinemaLocation, MaxNumOfSeats} ->
            io:format("[SHOW MONITOR] Received request for monitoring ~p \"~s\"~n", [Pid, ShowName]),
            Result = monitor(process, Pid),
            io:format("[SHOW MONITOR] Monitor request returned ~p~n", [Result]),
            NewMonitoredProcesses = maps:put(
                Pid, 
                {ShowId, ShowName, ShowDate, CinemaId, CinemaName, CinemaLocation, MaxNumOfSeats}, 
                MonitoredProcesses
            ),
            show_monitor_loop(NewMonitoredProcesses);
        
        {'DOWN', MonitorRef, process, Pid, normal} ->
            io:format(" [SHOW MONITOR] The process ~p is crashed with reason normal and with monitor ref ~p~n", [Pid, MonitorRef]),
            Tuple = maps:get(Pid, MonitoredProcesses, absent),
            io:format(" [SHOW MONITOR] Map get returns ~p~n", [Tuple]),
            NewMonitoredProcesses = maps:remove(Pid, MonitoredProcesses),
            show_monitor_loop(NewMonitoredProcesses);
        
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            io:format(" [SHOW MONITOR] The process ~p is crashed with reason ~p and with monitor ref ~p~n", [Pid, Reason, MonitorRef]),
            Tuple = maps:get(Pid, MonitoredProcesses, absent),
            io:format(" [SHOW MONITOR] The process to respawn is ~p~n", [Tuple]),
            case Tuple of
                {ShowId, ShowName, ShowDate, CinemaId, CinemaName, CinemaLocation, MaxNumOfSeats} ->
                    PidHandler = spawn( fun() -> 
                        show_handler:init_show_handler(ShowId, ShowName, ShowDate, CinemaId, CinemaName, CinemaLocation, MaxNumOfSeats)
                    end),
                    io:format(" [SHOW MONITOR] Process respawned with pid ~p~n", [PidHandler]),
                    MainEndpoint = whereis(main_server_endpoint),
                    MainEndpoint ! {respawned_handler, ShowId, PidHandler},
                    NewMonitoredProcesses = maps:remove(Pid, MonitoredProcesses),
                    self() ! {add_show_monitor, PidHandler, ShowId, ShowName, ShowDate, CinemaId, CinemaName, CinemaLocation, MaxNumOfSeats},
                    show_monitor_loop(NewMonitoredProcesses);
                absent ->
                    io:format(" [SHOW MONITOR] Error: Could not respawn process with pid ~p~n", [Pid]),
                    show_monitor_loop(MonitoredProcesses)
            end;
        
        _ ->
            io:format("[SHOW MONITOR] Unexpected message~n")
    end.