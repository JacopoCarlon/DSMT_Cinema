%%%-------------------------------------------------------------------
%%% @author nickrick3
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. jan 2025 11:25
%%%-------------------------------------------------------------------
-module(show_handler).
-author("nickrick3").

-export([init_show_handler/4]).


init_show_handler(ShowId, ShowName, Date, MaxNumOfSeats) ->
    CurrentTime = erlang:monotonic_time(second),
    case CurrentTime > Date of
        true -> io:format("[SHOW HANDLER] Invalid date at initialization~n");
        false ->
            % TIMERS
            DeathTimerDuartion = (Date - CurrentTime)*1000,
            erlang:send_after(DeathTimerDuartion, self(), {self(), kill_auction_suicide}),
            BackupClockDuration = 15 * 60 * 1000,
            erlang:send_after(BackupClockDuration, self(), {backup_clock}),
            
            % LOOP
            show_loop(ShowId, ShowName, Date, MaxNumOfSeats, 0, maps:new(), sets:new())
    end.



show_loop(ShowId, ShowName, Date, MaxNumOfSeats, AvailableSeats, BookingMap, ViewingCustomers) ->
    receive
        {Sender, restore_backup, BookingBackup} when is_map(BookingBackup) ->
            case Sender == whereis(main_server_endpoint) of
                false -> io:format("[SHOW HANDLER] Restore request ignored...~n");
                true ->
                    NewAvailableSeats = MaxNumOfSeats - lists:foldl(
                        fun(X, Sum) -> X + Sum end,
                        0,
                        map:values(BookingBackup)
                    ),
                    io:format(" [SHOW HANDLER] Restored Show bookings~n"),
                    show_loop(ShowId, ShowName, Date, MaxNumOfSeats, NewAvailableSeats, BookingBackup, ViewingCustomers)
            end;
        {Sender, kill_auction_suicide} ->
            case Sender == self() of
                false -> io:format("[SHOW HANDLER] Kill request ignored...~n");
                true ->
                    do_backup(ShowId, BookingMap),
                    io:format(" [SHOW HANDLER] Suicide: ~p killed...~n", [self()])
            end;
        {backup_clock} ->
            do_backup(ShowId, BookingMap),
            show_loop(ShowId, ShowName, Date, MaxNumOfSeats, AvailableSeats, BookingMap, ViewingCustomers);
        _ ->
            io:format("[SHOW HANDLER] Received unrecognized message. Ignoring it...~n"),
            show_loop(ShowId, ShowName, Date, MaxNumOfSeats, AvailableSeats, BookingMap, ViewingCustomers)
    end.


do_backup(ShowId, BookingMap) ->
    MainServerPid = whereis(main_server_endpoint),
    io:format("[SHOW HANDLER] Handler of Show ~s sending backup update to ~p.~n", [ShowId, MainServerPid]),
    MainServerPid ! {show_backup, ShowId, BookingMap}.