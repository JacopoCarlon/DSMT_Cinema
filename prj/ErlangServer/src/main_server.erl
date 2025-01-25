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
-export([init/1, handle_call/3]).


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

server_loop() ->
  receive
    {ClientPid, register, MessageMap} ->
      io:format("[MAIN SERVER] Received a register customer message~n"),
      Ret = register_customer(maps:get(username, MessageMap), maps:get(password, MessageMap)),
      ClientPid ! {self(), Ret};
    _ -> io:format("[MAIN SERVER] Received a message~n")
  end,
  server_loop().

register_customer(Username, Password) ->
  case gen_server:call(main_server, {register, Username, Password}) of
    {atomic, ok} -> {ok};
    _ -> {false}
  end.


%%%===================================================================
%%% Gen server callback functions
%%%===================================================================

init([]) ->
  database:start_database(),
  {ok, []}.

handle_call({register, Username, Password}, _From, _ServerState) ->
  Ret = database:add_customer(Username, Password),
  {reply, Ret, []}.