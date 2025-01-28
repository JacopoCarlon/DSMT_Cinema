%%%-------------------------------------------------------------------
%%% @author nickrick3
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. nov 2024 18:02
%%%-------------------------------------------------------------------
-module(database).
-author("nickrick3").

%% API
-export([create_database/0, start_database/0, stop_database/0]).
%% CRUD operations API
-export([
  add_cinema/3, get_cinema/1, find_cinema_by_name/1,
  add_customer/2, get_customer/1, get_customer_bookings/2,
  add_show/4, remove_show/1, get_show/1, get_show_pid/1, get_cinema_shows/1, update_show_bookings/2, update_show_pid/2
]).

%%%% TABLES
-record(cinema,  {cinema_id,
                  password,
                  name,
                  address}).


-record(customer,  {username,
                    password,
                    bookings}). %% set of {ShowId}


-record(show,  {showId,
                name,
                cinema_id,
                cinema_name,
                date,
                max_seats,
                pid,
                bookings}). %% definitive map of (CustomerUsername, NumOfSeats) pairs


%% Utilities
compose_cinema(CinemaId, Password, CinemaName, CinemaAddress) ->
  #cinema{
    cinema_id = CinemaId,
    password = Password,
    name     = CinemaName,
    address  = CinemaAddress
  }.

compose_customer(Username, Password, BookingSet) ->
  #customer{
    username = Username,
    password = Password,
    bookings = BookingSet
  }.

compose_show(ShowId, ShowName, CinemaId, CinemaName, Date, MaxSeats, Pid, BookingMap) ->
  #show{
    showId      = ShowId,
    name        = ShowName,
    cinema_id   = CinemaId,
    cinema_name = CinemaName,
    date        = Date,
    max_seats   = MaxSeats,
    pid         = Pid,
    bookings    = BookingMap
  }.

%% @doc Create Mnesia database
%% disc_copies means that records are stored also on disk
create_database() ->
  mnesia:create_schema([node()]),
  application:start(mnesia),
  io:format("[DATABASE] Database server started~n"),
  mnesia:create_table(cinema,
    [{attributes, record_info(fields, cinema)}, {type, ordered_set}, {disc_copies, [node()]}]),
  mnesia:create_table(customer,
    [{attributes, record_info(fields, customer)}, {disc_copies, [node()]}]),
  mnesia:create_table(show,
    [{attributes, record_info(fields, show)}, {type, ordered_set}, {index, [cinema_id]}, {disc_copies, [node()]}]).


%% @doc Start an existing Mnesia server
start_database() ->
  application:start(mnesia).

%% @doc Stop a running Mnesia server
stop_database() ->
  application:stop(mnesia).


%%%%%%%%%%%%%%% CRUD operations

%% CINEMA
add_cinema(Name, Password, Address) ->
  F = fun() ->
    io:format("[DATABASE] Searching last assigned ID...~n"),
    LastId = mnesia:last(cinema),
    io:format("[DATABASE] Check returned ~p~n", [LastId]),
    NewCinemaId = case LastId of
      '$end_of_table' -> 1;
      _               -> LastId +1
    end,
    io:format("[DATABASE] New CinemaId is ~p~n", [NewCinemaId]),
    mnesia:write(compose_cinema(NewCinemaId, Password, Name, Address)),
    NewCinemaId
  end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of add cinema is ~p~n", [Result]),
  Result.

get_cinema(CinemaId) ->
  F = fun() ->
        io:format("[DATABASE] Searching for cinema ID ~p~n", [CinemaId]),
        Match = #cinema{cinema_id='$1', password='$2', name='$3', address='$4',  _='_'},
        Guard = [{'==', '$1', CinemaId}],
        Result = ['$$'], %% return all fields
        mnesia:select(cinema, [{Match, Guard, Result}])
      end,
  mnesia:transaction(F).

find_cinema_by_name(CinemaName) ->
  F = fun() ->
    io:format("[DATABASE] Searching for cinema \"~s\"~n", [CinemaName]),
    Match = #cinema{cinema_id='$1', name='$2', address='$3', _='_'},
    Guard = [{'==', '$2', CinemaName}],
    Result = ['$$'], %% return list of {ID, Name, Address}
    mnesia:select(cinema, [{Match, Guard, Result}])
      end,
  mnesia:transaction(F).

%% CUSTOMER
add_customer(Username, Password) ->
  F = fun() ->
        io:format("[DATABASE] Checking if Username \"~s\" already exists~n", [Username]),
        Match = #customer{username='$1', _='_'},
        Guard = [{'==', '$1', Username}],
        Result = ['$1'],
        CheckResult = mnesia:select(customer, [{Match, Guard, Result}]),
        io:format("[DATABASE] Check returned ~p~n", [CheckResult]),
        case CheckResult==[] of
          true  ->  io:format("[DATABASE] Username available. Trying write~n"),
            mnesia:write(compose_customer(Username, Password, sets:new()));
          false ->  io:format("[DATABASE] Customer with same Username already exists~n"),
            false
        end
      end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of add customer is ~p~n", [Result]),
  Result.


get_customer(Username) ->
  F = fun() ->
    io:format("[DATABASE] Searching for Customer ~s~n", [Username]),
    Match = #customer{username='$1', password='$2', _ ='_' },
    Guard = [{'==', '$1', Username}],
    Result = [['$1', '$2']],
    mnesia:select(customer, [{Match, Guard, Result}])
      end,
  mnesia:transaction(F).


get_customer_bookings(Username, IncludeOldShows) ->
  Getter = fun(ShowId) ->
    [Show] = mnesia:read(show, ShowId),
    {{YY, MM, DD}, {H, M, _S}} = calendar:now_to_datetime(erlang:timestamp()),
    CurrentTime = io_lib:format("~4w-~2..0w-~2..0wT~2..0w:~2..0w", [YY, MM, DD, H, M]),
    case (CurrentTime < Show#show.date) or IncludeOldShows of
      true -> {true, {
          ShowId, 
          Show#show.name,
          Show#show.cinema_name,
          Show#show.date, 
          maps:get(Username, Show#show.bookings)
        }};
      false -> false
    end
  end,

  F = fun() ->
    io:format("[DATABASE] Searching list of shows booked by Customer ~s~n", [Username]),
    [Customer] = mnesia:read(customer, Username),
    lists:filtermap(Getter, sets:to_list(Customer#customer.bookings))
  end,
  mnesia:transaction(F).


%% SHOW
add_show(CinemaId, Name, Date, MaxSeats) ->
  F = fun() ->
    [Cinema] = mnesia:read(cinema, CinemaId),
    io:format("[DATABASE] Shearching last assigned Show ID~n"),
    LastId = mnesia:last(show),
    io:format("[DATABASE] Check returned ~p~n", [LastId]),
    NewShowId = case LastId of
      '$end_of_table' -> 1;
      _               -> LastId +1
    end,
    io:format("[DATABASE] New ShowId is ~p~n", [NewShowId]),
    mnesia:write(compose_show(NewShowId, Name, CinemaId, Cinema#cinema.name, Date, MaxSeats, 0, maps:new())),
    {NewShowId, Cinema#cinema.name}
  end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of add show is ~p~n", [Result]),
  Result.

remove_show(ShowId) ->
  F = fun() ->
    [Show] = mnesia:wread({show, ShowId}),
    lists:foreach(
      fun(Username) ->
        [Customer] = mnesia:wread({customer, Username}),
        BookingSet = sets:del_element(ShowId, Customer#customer.bookings),
        mnesia:write(Customer#customer{bookings = BookingSet})
      end,
      maps:keys(Show#show.bookings)
    ),
    mnesia:delete({show, ShowId})
  end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of delete show is ~p~n", [Result]),
  Result.

get_show(ShowId) ->
  F = fun() ->
        io:format("[DATABASE] Searching for show \"~p\"~n", [ShowId]),
        Match = #show{showId='$1', name='$2', cinema_id='$3', cinema_name='$4', date='$5', max_seats='$6',  _='_'},
        Guard = [{'==', '$1', ShowId}],
        Result = ['$$'], %% return all fields
        mnesia:select(show, [{Match, Guard, Result}])
      end,
  mnesia:transaction(F).

get_show_pid(ShowId) ->
  F = fun() ->
        io:format("[DATABASE] Searching for show \"~p\"~n", [ShowId]),
        Match = #show{showId='$1', pid='$2', _='_'},
        Guard = [{'==', '$1', ShowId}],
        Result = [['$2']], %% return pid
        mnesia:select(show, [{Match, Guard, Result}])
      end,
  mnesia:transaction(F).

get_cinema_shows(CinemaId) ->
  F = fun() ->
    io:format("[DATABASE] Searching for shows in Cinema \"~p\"~n", [CinemaId]),
    Match = #show{showId='$1', name='$2', date='$3', cinema_id='$4', max_seats='$5', _='_'},
    Guard = [{'==', '$4', CinemaId}],
    Result = [['$1', '$2', '$3', '$5']], %% return list of {ID, name, date, max_seats}
    mnesia:select(show, [{Match, Guard, Result}])
  end,
  mnesia:transaction(F).

update_show_bookings(ShowId, NewBookingMap) ->
  F = fun() ->
    io:format("[DATABASE] Updating list of bookers of show ~p~n", [ShowId]),
    [Show] = mnesia:wread({show, ShowId}),
    OldBookingMap = Show#show.bookings,
    
    ToRemove = maps:keys(maps:without(maps:keys(NewBookingMap), OldBookingMap)),
    lists:foreach(
      fun(Username) ->
        [Customer] = mnesia:wread({customer, Username}),
        BookingSet = sets:del_element(ShowId, Customer#customer.bookings),
        mnesia:write(Customer#customer{bookings = BookingSet})
      end,
      ToRemove
    ),

    ToAdd = maps:without(maps:keys(OldBookingMap), NewBookingMap),
    lists:foreach(
      fun(Username) ->
        [Customer] = mnesia:wread({customer, Username}),
        BookingSet = sets:add_element(ShowId, Customer#customer.bookings),
        mnesia:write(Customer#customer{bookings = BookingSet})
      end,
      ToAdd
    ),

    mnesia:write(Show#show{bookings = NewBookingMap})
  end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of update bookings is ~p~n", [Result]),
  Result.

%% update pid and return BoolingMap
update_show_pid(ShowId, NewPid) ->
  F = fun() ->
    io:format("[DATABASE] Updating pid of show ~p to ~p~n", [ShowId, NewPid]),
    [Show] = mnesia:wread({show, ShowId}),
    mnesia:write(Show#show{pid = NewPid}),
    Show#show.bookings
  end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of update show pid to customer is ~p~n", [Result]),
  Result.