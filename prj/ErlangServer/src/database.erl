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
  add_customer/2, get_customer/1, update_customer_bookings/2, get_customer_bookings/1,
  add_show/6, get_show/1, get_cinema_shows/1, update_show_bookings/2
]).

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
                pid,
                bookings = []}). %% definitive map of (CustomerUsername, NumOfSeats) pairs


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
                    mnesia:write(cinema:new(CinemaId, Password, Name, Address));
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
            mnesia:write(customer:new(Username, Password, []));
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
    Match = #customer{username='$1', _=''},
    Guard = [{'==', '$1', Username}],
    Result = ['$_'], %% return all fields
    mnesia:select(customer, [{Match, Guard, Result}])
      end,
  mnesia:transaction(F).


get_customer_bookings(Username) ->
  F = fun() ->
    io:format("[DATABASE] Searching list of shows booked by Customer ~s~n", [Username]),
    [Customer] = mnesia:read(customer, Username),
    lists:map(
      fun(ShowId) ->
          [Show] = mnesia:read(show, ShowId),
          CurrentTime = erlang:monotonic_time(second),
          if Show#show.date < CurrentTime -> %% old show
            %% return {old, ShowId, ShowName, Date, BookedSeats}
            {old_show, ShowId, Show#show.name, Show#show.date, maps:get(Username, Show#show.bookings)};
          else ->
            %% return {new, ShowId, ShowName, Date, Pid}
            {new, ShowId, Show#show.name, Show#show.date, Show#show.pid}
          end
      end,
      Customer#customer.bookings
    )
  end,
  mnesia:transaction(F).


update_customer_bookings(Username, ShowIdList) ->
  F = fun() ->
    io:format("[DATABASE] Updating list of shows booked by Customer ~s~n", Username),
    [Customer] = mnesia:wread(customer, Username),
    mnesia:write(Customer#customer{bookings = ShowIdList})
  end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of add booking to customer is ~p~n", [Result]),
  Result.


%% SHOW
add_show(ShowId, Name, CinemaId, Date, MaxSeats, Pid) ->
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
            mnesia:write(show:new(ShowId, Name, CinemaId, Date, MaxSeats, Pid, maps:new()));
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
    Match = #show{showId='$1', name='$2', date='$3', cinemaId='$4', pid='$5', _=''},
    Guard = [{'==', '$4', CinemaId}],
    Result = ['$1', '$2', '$3', '$5'], %% return list of {ID, name, date, pid}
    mnesia:select(show, [{Match, Guard, Result}])
      end,
  mnesia:transaction(F).

update_show_bookings(ShowId, BookingMap) ->
  F = fun() ->
    io:format("[DATABASE] Updating list of bookers of show ~s~n", ShowId),
    [Show] = mnesia:wread(show, ShowId),
    mnesia:write(Show#show{bookings = BookingMap})
  end,
  Result = mnesia:transaction(F),
  io:format("[DATABASE] Final result of add booking to customer is ~p~n", [Result]),
  Result.