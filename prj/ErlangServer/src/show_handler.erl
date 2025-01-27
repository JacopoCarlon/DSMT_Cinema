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

-export([init_show_handler/6]).
-include("macros.hrl").


init_show_handler(ShowId, ShowName, CinemaId, CinemaName, Date, MaxNumOfSeats) ->
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
            show_loop(
                #{
                    show_id     => ShowId, 
                    show_name   => ShowName,
                    cinema_id   => CinemaId, 
                    cinema_name => CinemaName, 
                    date        => Date,
                    max_seats   => MaxNumOfSeats, 
                    avail_seats => 0
                }, 
                maps:new(), 
                sets:new(), 
                false
            )
    end.



show_loop(ShowInfo, BookingMap, ViewingCustomers, ChangedBookings) ->
    receive
        {Client, new_viewer, MessageMap} ->
            io:format(" [SHOW HANDLER] Received new_viewer message~n"),
            NewViewingCustomers = sets:add_element(maps:get("username",MessageMap)),
            Client ! {self(), {ok, ShowInfo}},
            ?J_LISTENER ! {self(), update_show_state, ShowInfo, NewViewingCustomers, false},
            show_loop(ShowInfo, BookingMap, NewViewingCustomers, ChangedBookings);

        {Client, del_viewer, MessageMap} ->
            io:format(" [SHOW HANDLER] Received del_viewer message~n"),
            NewViewingCustomers = sets:del_element(maps:get("username",MessageMap)),
            Client ! {self(), {ok, ShowInfo}},
            ?J_LISTENER ! {self(), update_show_state, ShowInfo, NewViewingCustomers, false},
            show_loop(ShowInfo, BookingMap, NewViewingCustomers, ChangedBookings);
        
        {Client, update_booking, MessageMap} ->
            io:format(" [SHOW HANDLER] Received update_booking message~n"),
            Username = maps:get("username",MessageMap),
            NewBookedSeats = maps:get("num_seats", MessageMap),
            io:format(" [SHOW HANDLER] Customer ~s wants to change its booked seats to ~p.~n", [Username, NewBookedSeats]),
            {Success, NewAvailableSeats, NewBookingMap} = 
                do_new_booking(Username, NewBookedSeats, maps:get(avail_seats, ShowInfo), BookingMap),
            NewShowInfo = maps:put(avail_seats, NewAvailableSeats, ShowInfo),
            case Success of
                false -> Client ! {self(), {false}};
                true ->
                    Client ! {self(), {ok, NewShowInfo}},
                    ?J_LISTENER ! {self(), update_show_state, NewShowInfo, ViewingCustomers, false}
            end,    
            show_loop(NewShowInfo, NewBookingMap, ViewingCustomers, ChangedBookings or Success);
        
        {Sender, restore_backup, BookingBackup} ->
            case Sender == whereis(main_server_endpoint) of
                false -> 
                    io:format("[SHOW HANDLER] Restore request ignored...~n"),
                    show_loop(ShowInfo, BookingMap, ViewingCustomers, ChangedBookings);
                true ->
                    RestoredAvailableSeats = maps:get(max_seats, ShowInfo) - lists:foldl(
                        fun(X, Sum) -> X + Sum end,
                        0,
                        map:values(BookingBackup)
                    ),
                    io:format(" [SHOW HANDLER] Restored Show bookings~n"),
                    show_loop(
                        maps:put(avail_seats, RestoredAvailableSeats, ShowInfo), 
                        BookingBackup, 
                        ViewingCustomers, 
                        ChangedBookings
                    )
            end;
        
        {Sender, kill_auction_suicide} ->
            case Sender == self() of
                false -> io:format("[SHOW HANDLER] Kill request ignored...~n");
                true ->
                    do_backup(maps:get(show_id, ShowInfo), BookingMap, ChangedBookings),
                    ?J_LISTENER ! {self(), update_show_state, ShowInfo, ViewingCustomers, true},
                    io:format(" [SHOW HANDLER] Suicide: ~p killed...~n", [self()])
            end;
        
        {backup_clock} ->
            do_backup(maps:get(show_id, ShowInfo), BookingMap, ChangedBookings),
            show_loop(ShowInfo, BookingMap, ViewingCustomers, false);
        
        _ ->
            io:format("[SHOW HANDLER] Received unrecognized message. Ignoring it...~n"),
            show_loop(ShowInfo, BookingMap, ViewingCustomers, ChangedBookings)
    end.


do_new_booking(Username, 0, AvailableSeats, BookingMap) ->
    case maps:take(Username, BookingMap) of
        error -> {false, AvailableSeats, BookingMap};
        {OldBookedSeats, NewBookingMap} -> {true, AvailableSeats + OldBookedSeats, NewBookingMap}
    end;
do_new_booking(Username, NewBookedSeats, AvailableSeats, BookingMap) when is_number(NewBookedSeats) and NewBookedSeats > 0 ->
    OldBookedSeats = maps:get(Username, BookingMap, 0),
    SeatsDelta = NewBookedSeats - OldBookedSeats,
    case SeatsDelta < AvailableSeats of 
        false -> {false, AvailableSeats, BookingMap};
        true -> {true, AvailableSeats - SeatsDelta, maps:put(Username, NewBookedSeats, BookingMap)}
    end;
do_new_booking(_, _, AvailableSeats, BookingMap) ->
    {false, AvailableSeats, BookingMap}.


do_backup(ShowId, BookingMap, true) ->
    MainServerPid = whereis(main_server_endpoint),
    io:format("[SHOW HANDLER] Handler of Show ~s sending backup update to ~p.~n", [ShowId, MainServerPid]),
    MainServerPid ! {show_backup, ShowId, BookingMap};
do_backup(ShowId, _BookingMap, false) ->
    %% DO NOTHING
    io:format("[SHOW HANDLER] Backup of Show ~s is already up to date.~n", [ShowId]).