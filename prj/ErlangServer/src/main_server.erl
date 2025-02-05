%%%-------------------------------------------------------------------
%%% @author nickrick3
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(main_server).
-author("nickrick3").

-behaviour(gen_server).
-import(database, [create_database/0, start_database/0, add_customer/2]).

%% API
-export([start_server/0, server_loop/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-include("macros.hrl").

%%%===================================================================
%%% Spawning
%%%===================================================================

start_server() ->
  database:create_database(),
  Ret = gen_server:start({local, main_server}, ?MODULE, [], []),
  io:format("[MAIN SERVER] Start main server link: ~p~n", [Ret]),
  Pid = spawn(?MODULE, server_loop, []),
  register(main_server_endpoint, Pid),
  Ret.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Expected reply for correct operation (login, registration) from server: {self(), {true}}
%% Expected reply for any error on operation from server: {self(), {false}}
%% I.E.
%% The client will send a message like this: {#Pid<client@localhost.1.0>,login,#{"username" => "tizio","password" => "tizio"}}
%% The server will reply with {self(), {true}} if User "tizio" exists and the related pwd is "tizio", false otherwise
server_loop() ->
  receive
    %% Cinema
    {ClientPid, register_cinema, CinemaName, Password, CinemaAddress} ->
      io:format("[MAIN SERVER] Received a register cinema message~n"),
      Ret = register_cinema(CinemaName, Password, CinemaAddress),
      ClientPid ! {self(), Ret};
    
    {ClientPid, login_cinema, CinemaId, Password} ->
      io:format("[MAIN SERVER] Received a login cinema message~n"),
      Ret = login_cinema(CinemaId, Password),
      ClientPid ! {self(), Ret};

    {ClientPid, find_cinema_by_name, CinemaName} ->
      io:format("[MAIN SERVER] Received a find_cinema_by_name message~n"),
      Ret = find_cinema(CinemaName),
      ClientPid ! {self(), Ret};

    {ClientPid, get_cinema_shows, CinemaId} ->
      io:format("[MAIN SERVER] Received a get_cinema_shows message~n"),
      Ret = get_cinema_shows(CinemaId),
      ClientPid ! {self(), Ret};

    %% Customer
    {ClientPid, register_customer, Username, Password} ->
      io:format("[MAIN SERVER] Received a register customer message~n"),
      Ret = register_customer(Username, Password),
      ClientPid ! {self(), Ret};

    {ClientPid, login_customer, Username, Password} ->
      io:format("[MAIN SERVER] Received a login customer message~n"),
      Ret = login_customer(Username, Password),
      ClientPid ! {self(), Ret};

    {ClientPid, get_customer_bookings, Username} ->
      io:format("[MAIN SERVER] Received a get_customer_bookings message"),
      Ret = customer_bookings(Username),
      ClientPid ! {self(), Ret};

    %% Show
    {ClientPid, get_list_of_shows, IncludeOldShows} ->
      io:format("[MAIN SERVER] Received a get_list_of_shows message~n"),
      Ret = get_list_of_shows(IncludeOldShows),
      ClientPid ! {self(), Ret};

    {ClientPid, add_show, CinemaId, ShowMap} ->
      io:format("[MAIN SERVER] Received an add show message~n"),
      Ret = add_new_show(
        CinemaId, 
        maps:get("show_name", ShowMap),
        maps:get("show_date", ShowMap),
        maps:get("max_seats", ShowMap)
      ),
      ClientPid ! {self(), Ret};

    {ClientPid, get_show_pid, ShowId} ->
      io:format("[MAIN SERVER] Received a get_show_pid message~n"),
      Ret = get_show_pid(ShowId),
      ClientPid ! {self(), Ret};

    %% Messages from show monitor and handlers
    {show_backup, ShowId, UpdateMap, AvailableSeats, EndOfLife} ->
      io:format("[MAIN SERVER] Received a show_backup message~n"),
      _Ret = do_show_backup(ShowId, UpdateMap, AvailableSeats, EndOfLife);

    {respawned_handler, ShowId, PidHandler} ->
      io:format("[MAIN SERVER] Received a respawned_handler message~n"),
      _Ret = restore_show(ShowId, PidHandler);

    %% DEFAULT
    _ -> io:format("[MAIN SERVER] Received an unrecognized message~n")
  end,
  server_loop().


%% Cinema functions
register_cinema(CinemaName, Password, CinemaAddress) ->
  case gen_server:call(main_server, {add_cinema, CinemaName, Password, CinemaAddress}) of
    {atomic, NewCinemaId} -> {true, NewCinemaId};
    _ -> {false}
  end.

login_cinema(CinemaId, Password) ->
  Debug = gen_server:call(main_server, {get_cinema, CinemaId}),
  io:format("[DEBUG] Found ~p~n", [Debug]),
  case Debug of
    {atomic, [CinemaTuple | _]} -> {lists:nth(2, CinemaTuple) == Password}; 
    _ -> {false}
  end.

find_cinema(CinemaName) ->
  Debug = gen_server:call(main_server, {find_cinema, CinemaName}),
  io:format("[DEBUG] Found ~p~n", [Debug]),
  case Debug of
    {atomic, TupleList} -> {true, TupleList};
    _ -> {false}
  end.

get_cinema_shows(CinemaId) ->
  Debug = gen_server:call(main_server, {get_cinema_shows, CinemaId}),
  io:format("[DEBUG] Found ~p~n", [Debug]),
  case Debug of
    {atomic, TupleList} -> {true, TupleList}; 
    _ -> {false}
  end.

%% Customer functions
register_customer(Username, Password) ->
  case gen_server:call(main_server, {add_customer, Username, Password}) of
    {atomic, ok} -> {true};
    _ -> {false}
  end.

login_customer(Username, Password) ->
  case gen_server:call(main_server, {get_customer, Username}) of
    {atomic, [CustomerTuple | _]} -> {lists:nth(2, CustomerTuple) == Password};
    _ -> {false}
  end.

customer_bookings(Username) ->
  case gen_server:call(main_server, {get_customer_bookings, Username}) of
    {atomic, BookingsList} -> {true, BookingsList};
    _ -> {false}
  end.

%% Show Functions
get_list_of_shows(IncludeOldShows) ->
  case gen_server:call(main_server, {get_shows_list, IncludeOldShows}) of
    {atomic, ShowList} -> {true, ShowList};
    _ -> {false}
  end.

add_new_show(CinemaId, ShowName, ShowDate, MaxSeats) ->
  case gen_server:call(main_server, {new_show, CinemaId, ShowName, ShowDate, MaxSeats}) of
    {atomic, NewShowId, PidHandler} -> {true, NewShowId, PidHandler};
    _ -> {false}
  end.

get_show_pid(ShowId) ->
  case gen_server:call(main_server, {get_show_pid, ShowId}) of
    {atomic, [PidHandler | _]} -> {true, PidHandler};
    _ -> {false}
  end.

do_show_backup(ShowId, UpdateMap, AvailableSeats, EndOfLife) ->
  gen_server:call(main_server, {update_show_bookings, ShowId, UpdateMap, AvailableSeats, EndOfLife}).

restore_show(ShowId, PidHandler) ->
  case gen_server:call(main_server, {update_show_pid, ShowId, PidHandler}) of
    {atomic, BookingBackup} -> PidHandler ! {self(), restore_backup, BookingBackup};
    _ -> {false}
  end.

%%%===================================================================
%%% Gen server callback functions
%%%===================================================================

init([]) ->
  database:start_database(),
  {ok, []}.

%% cinema CRUD
handle_call({add_cinema, CinemaName, Password, CinemaAddress}, _From, _ServerState) ->
  Ret = database:add_cinema(CinemaName, Password, CinemaAddress),
  {reply, Ret, []};

handle_call({get_cinema, CinemaId}, _From, _ServerState) ->
  Ret = database:get_cinema(CinemaId),
  {reply, Ret, []};

handle_call({find_cinema, CinemaName}, _From, _ServerState) ->
  Ret = database:find_cinema_by_name(CinemaName),
  {reply, Ret, []};

% customer CRUD
handle_call({add_customer, Username, Password}, _From, _ServerState) ->
  Ret = database:add_customer(Username, Password),
  {reply, Ret, []};

handle_call({get_customer, Username}, _From, _ServerState) ->
  Ret = database:get_customer(Username),
  {reply, Ret, []};

handle_call({get_customer_bookings, Username}, _From, _ServerState) ->
  Ret = database:get_customer_bookings(Username, false),
  {reply, Ret, []};

% show CRUD
handle_call({get_shows_list, IncludeOldShows}, _From, _ServerState) ->
  Ret = database:get_shows_list(IncludeOldShows),
  {reply, Ret, []};

handle_call({get_cinema_shows, CinemaId}, _From, _ServerState) ->
  Ret = database:get_cinema_shows(CinemaId),
  {reply, Ret, []};

handle_call({new_show, CinemaId, ShowName, ShowDate, MaxSeats}, _From, _ServerState) ->
  AddRet = database:add_show(CinemaId, ShowName, ShowDate, MaxSeats),
  case AddRet of
    {atomic, NewShowId, CinemaName, CinemaLocation, false} -> 
      PidHandler = spawn(fun() -> 
        show_handler:init_show_handler(NewShowId, ShowName, ShowDate, CinemaId, CinemaName, CinemaLocation, MaxSeats) 
      end),
      case database:update_show_pid(NewShowId, PidHandler) of 
        {atomic, _BookingMap} ->
          ShowMonitorPid = whereis(show_monitor),
          ShowMonitorPid ! {add_show_monitor, PidHandler, NewShowId, ShowName, ShowDate, CinemaId, CinemaName, CinemaLocation, MaxSeats},
          {reply, {atomic, NewShowId}, []};
        _ ->
          io:format("[MAIN SERVER] Failed association of Show with Process. Rollback...~n"),
          exit(PidHandler, kill),
          database:remove_show(NewShowId),
          {reply, {false}, []}
      end;
    {atomic, NewShowId, _CinemaName, _CinemaLocation, true} -> {reply, {atomic, NewShowId}, []};
    _ -> {reply, {false}, []}
  end;

handle_call({get_show_pid, ShowId}, _From, _ServerState) ->
  Ret = database:get_show_pid(ShowId),
  {reply, Ret, []};

handle_call({update_show_bookings, ShowId, UpdateMap, AvailableSeats, EndOfLife}, _From, _ServerState) ->
  database:update_show_bookings(ShowId, UpdateMap, AvailableSeats, EndOfLife);

handle_call({update_show_pid, ShowId, PidHandler}, _From, _ServerState) ->
  Ret = database:update_show_pid(ShowId, PidHandler),
  {reply, Ret, []}.




handle_cast(reset, ServerState) ->
  {noreply, ServerState}.           % general format: {noreply, NewState}