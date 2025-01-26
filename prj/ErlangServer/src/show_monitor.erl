%%%-------------------------------------------------------------------
%%% @author nickrick3
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. jan 2025 12:14
%%%-------------------------------------------------------------------
-module(show_handler).
-author("nickrick3").

-export([start_show_monitor/0]).


start_show_monitor() ->
    io:format("[SHOW MONITOR] Show Monitor started with pid ~p~n", [self()]),
    register(show_monitor, self()),
    show_monitor_loop(maps:new()).


show_monitor_loop(MonitoredProcesses) ->
    receive
        {monitor_new_show, ShowPid, ShowId, ShowName, Date, MaxNumOfSeats} ->
            io:format("[SHOW MONITOR] Received request for monitoring ~p \"~s\"~n", [ShowPid, ShowName]),
            Result = monitor(process, ShowPid),
            io:format("[SHOW MONITOR] Monitor request returned ~p~n", [Result]),
            NewMonitoredProcesses = maps:put(ShowPid, {ShowId, ShowName, Date, MaxNumOfSeats}, MonitoredProcesses),
            show_monitor_loop(NewMonitoredProcesses);
        _ ->
            io:format("[SHOW MONITOR] Unexpected message~n")
    end.