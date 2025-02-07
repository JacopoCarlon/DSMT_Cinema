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

-export([init_show_handler/7]).
-include("macros.hrl").


init_show_handler(ShowId, ShowName, ShowDate, CinemaId, CinemaName, CinemaLocation, MaxNumOfSeats) ->
    CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(erlang:timestamp())),
    DateSeconds = timestring_to_seconds(ShowDate),
    case CurrentTimeSeconds > DateSeconds of
        true -> io:format("[SHOW HANDLER] Invalid date at initialization~n");
        false ->
            % TIMERS
            DeathTimerDuartion = (DateSeconds - CurrentTimeSeconds)*1000,
            erlang:send_after(DeathTimerDuartion, self(), {self(), kill_auction_suicide}),
            BackupClockDuration = 15 * 60 * 1000,
            erlang:send_after(BackupClockDuration, self(), {backup_clock}),
            
            % LOOP
            show_loop(
                #{
                    show_id         => ShowId, 
                    show_name       => ShowName,
                    show_date       => ShowDate,
                    cinema_id       => CinemaId, 
                    cinema_name     => CinemaName, 
                    cinema_location => CinemaLocation,
                    max_seats       => MaxNumOfSeats
                },
                MaxNumOfSeats,
                maps:new(),
                maps:new()
            )
    end.



show_loop(StaticInfo, AvailableSeats, CommittedBookings, WaitingBookings) ->
    receive
        {Client, get_data_for_customer, Username} ->
            io:format(" [SHOW HANDLER] Customer '~p' asked for show data~n", [Username]),
            Client ! construct_msg_for_customer(
                true, Username, StaticInfo, AvailableSeats, CommittedBookings, WaitingBookings
            ),
            show_loop(StaticInfo, AvailableSeats, CommittedBookings, WaitingBookings);
        
        {Client, get_data_for_cinema, CinemaId} ->
            io:format(" [SHOW HANDLER] Cinema with ID ~p asked for show data~n", [CinemaId]),
            Client ! construct_message(
                StaticInfo, AvailableSeats, CommittedBookings, WaitingBookings
            ),
            show_loop(StaticInfo, AvailableSeats, CommittedBookings, WaitingBookings);
        

        {Client, update_booking, MessageMap} ->
            io:format(" [SHOW HANDLER] Received update_booking message~n"),
            Username = maps:get("username",MessageMap),
            NewBookedSeats = maps:get("num_seats", MessageMap),
            io:format(" [SHOW HANDLER] Customer ~s wants to change its booked seats to ~p.~n", [Username, NewBookedSeats]),
            LastCommittedSeats = maps:get(Username, CommittedBookings, 0),
            {Success, NewAvailableSeats, NewWaitingBookings} = 
                do_new_booking(Username, NewBookedSeats, LastCommittedSeats, WaitingBookings, AvailableSeats),
            
            Client ! construct_msg_for_customer(
                Success, Username, StaticInfo, NewAvailableSeats, CommittedBookings, NewWaitingBookings
            ),
            % ?J_LISTENER ! {self(), update_show_state, NewShowInfo, ViewingCustomers, false}
            show_loop(StaticInfo, NewAvailableSeats, CommittedBookings, NewWaitingBookings);
        
        {Sender, restore_backup, SeatsBackup, BookingBackup} ->
            case Sender == whereis(main_server_endpoint) of
                false -> 
                    io:format("[SHOW HANDLER] Restore request ignored...~n"),
                    show_loop(StaticInfo, AvailableSeats, CommittedBookings, WaitingBookings);
                true ->
                    io:format(" [SHOW HANDLER] Restored Show bookings~n"),
                    show_loop(StaticInfo, SeatsBackup, BookingBackup, maps:new())
            end;
        
        {Sender, kill_auction_suicide} ->
            case Sender == self() of
                false -> 
                    io:format("[SHOW HANDLER] Kill request ignored...~n"),
                    show_loop(StaticInfo, AvailableSeats, CommittedBookings, WaitingBookings);
                true ->
                    do_backup(maps:get(show_id, StaticInfo), CommittedBookings,  AvailableSeats, WaitingBookings, true),
                    %% ?J_LISTENER ! {self(), update_show_state, ShowInfo, ViewingCustomers, true},
                    io:format(" [SHOW HANDLER] Suicide: ~p killed...~n", [self()])
            end;
        
        {backup_clock} ->
            NewCommittedMap = 
                do_backup(maps:get(show_id, StaticInfo), CommittedBookings,  AvailableSeats, WaitingBookings, false),
            show_loop(StaticInfo, AvailableSeats, NewCommittedMap, maps:new());
        
        Message ->
            io:format("[SHOW HANDLER] Received unrecognized message: ~p~n. Ignoring it...~n", [Message]),
            show_loop(StaticInfo, AvailableSeats, CommittedBookings, WaitingBookings)
    end.


%%% BOOKING UTILITIES
% Cancel booking, no committed value
do_new_booking(Username, 0, 0, WaitingBookingMap, AvailableSeats) ->
    case maps:take(Username, WaitingBookingMap) of
        error -> {false, AvailableSeats, WaitingBookingMap};
        {OldBookedSeats, NewMap} -> {true, AvailableSeats + OldBookedSeats, NewMap}
    end;

% Cancel booking, committed value
do_new_booking(Username, 0, LastCommittedSeats, WaitingBookingMap, AvailableSeats) 
  when is_number(LastCommittedSeats) and LastCommittedSeats > 0 ->

    case maps:get(Username, WaitingBookingMap, none) of
        none -> {true, AvailableSeats + LastCommittedSeats, maps:put(Username, 0, WaitingBookingMap)};
        OldValue -> {true, AvailableSeats + OldValue, maps:put(Username, 0, WaitingBookingMap)}
    end;

% Update booking
do_new_booking(Username, NewBookedSeats, LastCommittedSeats, WaitingBookingMap, AvailableSeats) 
  when is_number(NewBookedSeats) and NewBookedSeats and is_number(LastCommittedSeats) and LastCommittedSeats > 0 ->
    
    OldRelevantValue = case WaitingBookingMap = maps:get(Username, WaitingBookingMap, none) of
        none -> LastCommittedSeats;
        TempValue -> TempValue
    end,
    SeatsDelta = NewBookedSeats - OldRelevantValue,
    case SeatsDelta < AvailableSeats of 
        false -> {false, AvailableSeats, WaitingBookingMap};
        true -> {true, AvailableSeats - SeatsDelta, maps:put(Username, NewBookedSeats, WaitingBookingMap)}
    end;

% Do nothing
do_new_booking(_, _, _, WaitingBookingMap, AvailableSeats) ->
    {false, AvailableSeats, WaitingBookingMap}.


%%% MESSAGE UTILITIES
% message for customers
construct_msg_for_customer(ResponseCode, Username, StaticInfo, AvailableSeats, CommittedBookings, WaitingBookingMap) ->
    CommittedValue = maps:get(Username, CommittedBookings, 0),
    MessageList = [
        maps:get(show_id, StaticInfo),
        maps:get(show_name, StaticInfo),
        maps:get(show_date, StaticInfo),
        maps:get(cinema_id, StaticInfo), 
        maps:get(cinema_name, StaticInfo), 
        maps:get(cinema_location, StaticInfo),
        maps:get(max_seats, StaticInfo),
        AvailableSeats,
        [{Username, CommittedValue}]
    ],
    case maps:get(Username, WaitingBookingMap, none) of
        none -> {self(), {ResponseCode, MessageList}};
        UncommittedValue -> {self(), {ResponseCode, lists:append(MessageList, [[{Username, UncommittedValue}]])}}
    end.

% message for others
construct_message(StaticInfo, AvailableSeats, CommittedBookings, WaitingBookingMap) ->
    {
        self(),
        {
            true,
            [
                maps:get(show_id, StaticInfo),
                maps:get(show_name, StaticInfo),
                maps:get(show_date, StaticInfo),
                maps:get(cinema_id, StaticInfo), 
                maps:get(cinema_name, StaticInfo), 
                maps:get(cinema_location, StaticInfo),
                maps:get(max_seats, StaticInfo),
                AvailableSeats,
                maps:to_list(CommittedBookings),
                maps:to_list(WaitingBookingMap)
            ]
        }
    }.

%%% BACKUP UTILITIES
% No changes
do_backup(ShowId, CommittedBookings, #{}, _AvailableSeats, false) ->
    %% DO NOTHING
    io:format("[SHOW HANDLER] Backup of Show ~p is already up to date.~n", [ShowId]),
    CommittedBookings;

% Changes and/or end of life
do_backup(ShowId, CommittedBookings, WaitingBookings,  AvailableSeats, EndOfLife) ->
    %% Notify main server
    MainServerPid = whereis(main_server_endpoint),
    io:format("[SHOW HANDLER] Handler of Show ~p sending end_of_life update to ~p.~n", [ShowId, MainServerPid]),
    MainServerPid ! {show_backup, ShowId, WaitingBookings, AvailableSeats, EndOfLife},

    %% Update committed values
    maps:filter(
        fun(_Key, Value) -> Value > 0 end, 
        maps:merge(CommittedBookings, WaitingBookings)
    ).

%%% OTHER UTILITIES
timestring_to_seconds(String) ->
    [Date, Time] = string:tokens(String, "T"),
    [YY, MM, DD] = string:tokens(Date, "-"),
    [HH, Min]    = string:tokens(Time, ":"),
    calendar:datetime_to_gregorian_seconds(
        {{
            list_to_integer(YY),
            list_to_integer(MM),
            list_to_integer(DD)
        },{
            list_to_integer(HH),
            list_to_integer(Min),
            0
        }}
    ).
    