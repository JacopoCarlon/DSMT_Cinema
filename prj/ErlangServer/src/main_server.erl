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

%% Expected reply for correct operation (login, registration) from server: {self(), {ok}}
%% Expected reply for any error on operation from server: {self(), {false}}
%% I.E.
%% The client will send a message like this: {#Pid<client@localhost.1.0>,login,#{"username" => "tizio","password" => "tizio"}}
%% The server will reply with {self(), {ok}} if User "tizio" exists and the related pwd is "tizio", false otherwise
server_loop() ->
  receive
    %% Cinema
    {ClientPid, register_cinema, MessageMap} ->
      io:format("[MAIN SERVER] Received a register cinema message~n"),
      Ret = register_cinema(
        maps:get("cinema_id", MessageMap), 
        maps:get("password", MessageMap),
        maps:get("cinema_name", MessageMap),
        maps:get("cinema_addr", MessageMap)
      ),
      ClientPid ! {self(), Ret};
    
    {ClientPid, login_cinema, MessageMap} ->
      io:format("[MAIN SERVER] Received a login cinema message~n"),
      Ret = login_cinema(
        maps:get("cinema_id", MessageMap), 
        maps:get("password", MessageMap)
      ),
      ClientPid ! {self(), Ret};

    %% Customer
    {ClientPid, register_customer, MessageMap} ->
      io:format("[MAIN SERVER] Received a register customer message~n"),
      Ret = register_customer(
        maps:get("username", MessageMap), 
        maps:get("password", MessageMap)
      ),
      ClientPid ! {self(), Ret};

    {ClientPid, login_customer, MessageMap} ->
      io:format("[MAIN SERVER] Received a login cinema message~n"),
      Ret = login_customer(
        maps:get("username", MessageMap), 
        maps:get("password", MessageMap)
      ),
      ClientPid ! {self(), Ret};

    %% Messages from show monitor and handlers
    {respawned_handler, ShowId, PidHandler} ->
      io:format("[MAIN SERVER] Received a respawned_handler message~n"),
      _Ret = restore_show(ShowId, PidHandler);

    %% DEFAULT
    _ -> io:format("[MAIN SERVER] Received a message~n")
  end,
  server_loop().


%% Cinema functions
register_cinema(CinemaId, Password, CinemaName, Address) ->
  case gen_server:call(main_server, {register_cinema, CinemaId, Password, CinemaName, Address}) of
    {atomic, ok} -> {ok};
    _ -> {false}
  end.

login_cinema(CinemaId, Password) ->
  case gen_server:call(main_server, {add_cinema, CinemaId}) of
    {atomic, [CinemaTuple | _]} -> 
      case lists:nth(2, CinemaTuple) == Password of
        true  -> {ok};
        false -> {false}
      end;
    _ -> {false}
  end.


%% Customer functions
register_customer(Username, Password) ->
  case gen_server:call(main_server, {add_customer, Username, Password}) of
    {atomic, ok} -> {ok};
    _ -> {false}
  end.

login_customer(Username, Password) ->
  case gen_server:call(main_server, {get_customer, Username}) of
    {atomic, [CustomerTuple | _]} -> 
      case lists:nth(2, CustomerTuple) == Password of
        true  -> {ok};
        false -> {false}
      end;
    _ -> {false}
  end.

%% Show Functions
restore_show(ShowId, PidHandler) ->
  case gen_server:call(main_server, {update_show_pid, ShowId, PidHandler}) of
    {atomic, BookingBackup} -> 
      PidHandler ! {self(), restore_backup, BookingBackup};
    _ -> {false}
  end.

%%%===================================================================
%%% Gen server callback functions
%%%===================================================================

init([]) ->
  database:start_database(),
  {ok, []}.

handle_call({add_cinema, CinemaId, Password, CinemaName, Address}, _From, _ServerState) ->
  Ret = database:add_cinema(CinemaId, Password, CinemaName, Address),
  {reply, Ret, []};

handle_call({get_cinema, CinemaId}, _From, _ServerState) ->
  Ret = database:get_cinema(CinemaId),
  {reply, Ret, []}; 

handle_call({add_customer, Username, Password}, _From, _ServerState) ->
  Ret = database:add_customer(Username, Password),
  {reply, Ret, []};

handle_call({get_customer, Username}, _From, _ServerState) ->
  Ret = database:get_customer(Username),
  {reply, Ret, []};

handle_call({update_show_pid, ShowId, PidHandler}, _From, _ServerState) ->
  Ret = database:update_show_pid(ShowId, PidHandler),
  {reply, Ret, []}. 









handle_cast(reset, ServerState) ->
  {noreply, ServerState}.           % general format: {noreply, NewState}