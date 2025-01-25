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
  add_cinema/4, get_cinema/1, find_cinema_by_name/1,
  add_customer/2, get_customer/1, add_booking_to_customer/3, remove_booking/3,
  add_show/6, get_show/1, get_cinema_shows/1]
).

%%%% TABLES
-record(cinema,  {cinemaId,
                  password,
                  name,
                  address}).


-record(customer,  {username,
                    password,
                    bookings = []}). %% list of {ShowId}


-record(show,  {showId,
                name,
                cinemaId,
                date,
                maxSeats,
                bookings = []}). %% list of {CustomerUsername, NumOfSeats} tuples

%%%% utility record
-record(booking, {customer, numOfSeats}).

%% @doc Create Mnesia database
%% disc_copies means that records are stored also on disk
create_database() ->
  mnesia:create_schema([node()]),
  application:start(mnesia),
  io:format("[DATABASE] Database server started~n"),
  mnesia:create_table(cinema,
    [{attributes, record_info(fields, cinema)}, {disc_copies, [node()]}]),
  mnesia:create_table(customer,
    [{attributes, record_info(fields, customer)}, {disc_copies, [node()]}]),
  mnesia:create_table(show,
    [{attributes, record_info(fields, show)}, {index, [cinemaId]}, {disc_copies, [node()]}]).


%% @doc Start an existing Mnesia server
start_database() ->
  application:start(mnesia).

%% @doc Stop a running Mnesia server
stop_database() ->
  application:stop(mnesia).


%%%%%%%%%%%%%%% Utilities
booking_tuple(CustomerUsername, NumOfSeats) ->
  #booking(
    customer    = CustomerUsername,
    numOfSeats  = NumOfSeats
  ).

cinema_tuple(CinemaId, Password, Name, Address) ->
  #cinema(
    cinemaId  = CinemaId,
    password  = Password,
    name      = Name,
    address   = Address
  ).

customer_tuple(Username, Password, Bookings) ->
  #customer(
    username = Username,
    password = Password,
    bookings = Bookings
  ).

show_tuple(ShowId, Name, CinemaId, Date, MaxSeats, Bookings) ->
  #show(
    showId    = ShowId,
    name      = Name,
    cinemaId  = CinemaId,
    date      = Date,
    maxSeats  = MaxSeats,
    bookings  = Bookings
  ).

%%%%%%%%%%%%%%% CRUD operations

%% CINEMA
add_cinema(CinemaId, Password, Name, Address) ->
  F = fun() ->
        io:format("[DATABASE] Checking if ID ~s already exists~n", [CinemaId]),
        Match = #cinema{cinemaId='$1', _='_'},
        Guard = [{'==', '$1', CinemaId}],
        Result = ['$1'],
        CheckResult = mnesia:select(cinema, [{Match, Guard, Result}]),
        io:format("[DATABASE] Check returned ~p~n", [CheckResult]),
        case CheckResult==[] of
          true  ->  io:format("[DATABASE] ID available. Trying write~n"),
                    mnesia:write(cinema_tuple(CinemaId, Password, Name, Address, []));
          false ->  io:format("[DATABASE] Cinema with same ID already exists~n"),
                    false
        end
      end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of add cinema is ~p~n", [Result]),
  Result.

get_cinema(CinemaId) ->
  F = fun() ->
        io:format("[DATABASE] Searching for cinema ID ~s~n", [CinemaId]),
        Match = #cinema{cinemaId='$1', _=''},
        Guard = [{'==', '$1', CinemaId}],
        Result = ['$_'], %% return all fields
        mnesia:select(cinema, [{Match, Guard, Result}])
      end,
  mnesia:transaction(F).

find_cinema_by_name(CinemaName) ->
  F = fun() ->
    io:format("[DATABASE] Searching for cinema \"~s\"~n", [CinemaName]),
    Match = #cinema{cinemaId='$1', name='$2', address='$3', _=''},
    Guard = [{'==', '$1', CinemaName}],
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
            mnesia:write(customer_tuple(Username, Password, []));
          false ->  io:format("[DATABASE] Customer with same Username already exists~n"),
            false
        end
      end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of add customer is ~p~n", [Result]),
  Result.

get_customer(Username) ->
  F = fun() ->
    io:format("[DATABASE] Searching for customer \"~s\"~n", [Username]),
    Match = #customer{username='$1', _=''},
    Guard = [{'==', '$1', Username}],
    Result = ['$_'], %% return all fields
    mnesia:select(customer, [{Match, Guard, Result}])
      end,
  mnesia:transaction(F).


add_booking_to_customer(Username, ShowId, NumOfSeats) ->
  F = fun() ->
    io:format("[DATABASE] Adding new boooking of ~i seats for Show ~s to Customer ~s~n", NumOfSeats, ShowId, Username),
    [Customer] = mnesia:read(customer, Username),
    case lists:keytake(ShowId, #booking.showId, Customer#customer.bookings) of
      {value, OldBooking, _List} ->
        NewBooking = booking_tuple(ShowId, NumOfSeats + OldBooking#booking.numOfSeats);
      false ->
        NewBooking = booking_tuple(ShowId, NumOfSeats)
    end,
    NewList = lists:keystore(
      ShowId,                       % value of the key
      #booking.showId,              % position of the key
      Customer#customer.bookings,   % old tuple list
      NewBooking                    % new tuple
    ),
    mnesia:write(Customer#customer{bookings = NewList})
  end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of add booking to customer is ~p~n", [Result]),
  Result.

remove_booking(Username, ShowId, NumOfSeats) ->
  F = fun() ->
    io:format("[DATABASE] Removing ~i booked seats of Show ~s from Customer ~s~n", NumOfSeats, ShowId, Username),
    [Customer] = mnesia:read(customer, Username),
    case lists:keytake(ShowId, #booking.showId, Customer#customer.bookings) of
      {value, OldBooking, _List} ->
        NewSeats = OldBooking#booking.numOfSeats - NumOfSeats,
        case (NewSeats > 0) of
          true ->
            NewList = lists:keystore(
              ShowId,                         % value of the key
              #booking.showId,                % position of the key
              Customer#customer.bookings,     % old tuple list
              booking_tuple(ShowId, NewSeats) % new tuple
            );
          false ->
            NewList = lists:keydelete(
              ShowId,                         % value of the key
              #booking.showId,                % position of the key
              Customer#customer.bookings      % old tuple list
            )
        end,
        mnesia:write(Customer#customer{bookings = NewList});
      false ->
        io:format("[DATABASE] Searched booking does not exists ~n")
    end
  end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of remove booking from customer is ~p~n", [Result]),
  Result.

%% SHOW
add_show(ShowId, Name, CinemaId, CinemaName, Date, MaxSeats) ->
  F = fun() ->
        io:format("[DATABASE] Checking if Show ID ~s already exists~n", [ShowId]),
        Match = #show{showId='$1', _='_'},
        Guard = [{'==', '$1', ShowId}],
        Result = ['$1'],
        CheckResult = mnesia:select(show, [{Match, Guard, Result}]),
        io:format("[DATABASE] Check returned ~p~n", [CheckResult]),
        case CheckResult==[] of
          true  ->
            io:format("[DATABASE] Show ID available. Trying write~n"),
            mnesia:write(show_tuple(ShowId, Name, CinemaName, Date, MaxSeats));
          false ->
            io:format("[DATABASE] Show with same ID already exists~n"),
            false
        end
      end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of add show is ~p~n", [Result]),
  Result.

get_show(ShowId) ->
  F = fun() ->
        io:format("[DATABASE] Searching for show \"~s\"~n", [ShowId]),
        Match = #show{showId='$1', _=''},
        Guard = [{'==', '$1', ShowId}],
        Result = ['$_'], %% return all fields
        mnesia:select(show, [{Match, Guard, Result}])
      end,
  mnesia:transaction(F).

get_cinema_shows(CinemaId) ->
  F = fun() ->
    io:format("[DATABASE] Searching for shows in Cinema \"~s\"~n", [CinemaId]),
    Match = #show{showId='$1', name='$2', date='$3', cinemaId='$4', _=''},
    Guard = [{'==', '$4', CinemaId}],
    Result = ['$1', '$2', '$3'], %% return list of {ID, name, date}
    mnesia:select(show, [{Match, Guard, Result}])
      end,
  mnesia:transaction(F).