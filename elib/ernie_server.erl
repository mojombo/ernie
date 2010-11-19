-module(ernie_server).
-behaviour(gen_server).
-include_lib("ernie.hrl").

%% api
-export([start_link/1, start/1, process/1, kick/0, fin/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start(Args) ->
  gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

process(Sock) ->
  gen_server:cast(?MODULE, {process, Sock}).

kick() ->
  gen_server:cast(?MODULE, kick).

fin() ->
  gen_server:cast(?MODULE, fin).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Port, Configs]) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("~p starting~n", [?MODULE]),
  {ok, LSock} = try_listen(Port, 500),
  spawn(fun() -> loop(LSock) end),
  Map = init_map(Configs),
  io:format("pidmap = ~p~n", [Map]),
  {ok, #state{lsock = LSock, map = Map}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({process, Sock}, State) ->
  Log = #log{hq = queue:len(State#state.hq),
             lq = queue:len(State#state.lq),
             taccept = erlang:now()},
  Request = #request{sock = Sock, log = Log},
  State2 = receive_term(Request, State),
  {noreply, State2};
handle_cast(kick, State) ->
  case queue:out(State#state.hq) of
    {{value, Request}, Hq2} ->
      State2 = process_request(Request, hq, Hq2, State),
      {noreply, State2};
    {empty, _Hq} ->
      case queue:out(State#state.lq) of
        {{value, Request}, Lq2} ->
          State2 = process_request(Request, lq, Lq2, State),
          {noreply, State2};
        {empty, _Lq} ->
          {noreply, State}
      end
  end;
handle_cast(fin, State) ->
  Listen = State#state.listen,
  Count = State#state.count,
  ZCount = State#state.zcount + 1,
  logger:debug("Fin; Listen = ~p (~p/~p)~n", [Listen, Count, ZCount]),
  case Listen =:= false andalso ZCount =:= Count of
    true -> halt();
    false -> {noreply, State#state{zcount = ZCount}}
  end;
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(Msg, State) ->
  error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
  {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal
%%====================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module mapping

init_map(Configs) ->
  lists:map((fun extract_mapping/1), Configs).

extract_mapping(Config) ->
  Id = proplists:get_value(id, Config),
  Mod = proplists:get_value(module, Config),
  {Mod, Id}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Listen and loop

try_listen(Port, 0) ->
  error_logger:error_msg("Could not listen on port ~p~n", [Port]),
  {error, "Could not listen on port"};
try_listen(Port, Times) ->
  Res = gen_tcp:listen(Port, [binary, {packet, 4}, {active, false}, {reuseaddr, true}, {backlog, 128}]),
  case Res of
    {ok, LSock} ->
      error_logger:info_msg("Listening on port ~p~n", [Port]),
      % gen_tcp:controlling_process(LSock, ernie_server),
      {ok, LSock};
    {error, Reason} ->
      error_logger:info_msg("Could not listen on port ~p: ~p~n", [Port, Reason]),
      timer:sleep(5000),
      try_listen(Port, Times - 1)
  end.

loop(LSock) ->
  case gen_tcp:accept(LSock) of
    {error, closed} ->
      logger:debug("Listen socket closed~n", []),
      timer:sleep(infinity);
    {error, Error} ->
      logger:debug("Connection accept error: ~p~n", [Error]),
      loop(LSock);
    {ok, Sock} ->
      logger:debug("Accepted socket: ~p~n", [Sock]),
      ernie_server:process(Sock),
      loop(LSock)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Receive and process

receive_term(Request, State) ->
  Sock = Request#request.sock,
  case gen_tcp:recv(Sock, 0) of
    {ok, BinaryTerm} ->
      logger:debug("Got binary term: ~p~n", [BinaryTerm]),
      Term = binary_to_term(BinaryTerm),
      logger:info("Got term: ~p~n", [Term]),
      case Term of
        {call, '__admin__', Fun, Args} ->
          ernie_admin:process(Sock, Fun, Args, State);
        {info, Command, Args} ->
          Infos = Request#request.infos,
          Infos2 = [BinaryTerm | Infos],
          Request2 = Request#request{infos = Infos2},
          Request3 = process_info(Request2, Command, Args),
          receive_term(Request3, State);
        _Any ->
          Request2 = Request#request{action = BinaryTerm},
          close_if_cast(Term, Request2),
          case Request2#request.priority of
            high ->
              Hq2 = queue:in(Request2, State#state.hq),
              Lq2 = State#state.lq;
            low ->
              Hq2 = State#state.hq,
              Lq2 = queue:in(Request2, State#state.lq)
          end,
          ernie_server:kick(),
          State#state{hq = Hq2, lq = Lq2}
      end;
    {error, closed} ->
      ok = gen_tcp:close(Sock),
      State
  end.

process_info(Request, priority, [Priority]) ->
  Request#request{priority = Priority};
process_info(Request, _Command, _Args) ->
  Request.

process_request(Request, Priority, Q2, State) ->
  ActionTerm = bert:decode(Request#request.action),
  {_Type, Mod, _Fun, _Args} = ActionTerm,
  Specs = lists:filter(fun({X, _Id}) -> Mod =:= X end, State#state.map),
  case Specs of
    [] -> no_module(Mod, Request, Priority, Q2, State);
    _Else -> process_module(ActionTerm, Specs, Request, Priority, Q2, State)
  end.

no_module(Mod, Request, Priority, Q2, State) ->
  logger:debug("No such module ~p~n", [Mod]),
  Sock = Request#request.sock,
  Class = <<"ServerError">>,
  Message = list_to_binary(io_lib:format("No such module '~p'", [Mod])),
  gen_tcp:send(Sock, term_to_binary({error, [server, 0, Class, Message, []]})),
  ok = gen_tcp:close(Sock),
  finish(Priority, Q2, State).

process_module(ActionTerm, [], Request, Priority, Q2, State) ->
  {_Type, Mod, Fun, _Args} = ActionTerm,
  logger:debug("No such function ~p:~p~n", [Mod, Fun]),
  Sock = Request#request.sock,
  Class = <<"ServerError">>,
  Message = list_to_binary(io_lib:format("No such function '~p:~p'", [Mod, Fun])),
  gen_tcp:send(Sock, term_to_binary({error, [server, 0, Class, Message, []]})),
  ok = gen_tcp:close(Sock),
  finish(Priority, Q2, State);
process_module(ActionTerm, Specs, Request, Priority, Q2, State) ->
  [{_Mod, Id} | OtherSpecs] = Specs,
  case Id of
    native ->
      logger:debug("Dispatching to native module~n", []),
      {_Type, Mod, Fun, Args} = ActionTerm,
      case erlang:function_exported(Mod, Fun, length(Args)) of
        false ->
          logger:debug("Not found in native module ~p~n", [Mod]),
          process_module(ActionTerm, OtherSpecs, Request, Priority, Q2, State);
        true ->
          PredFun = list_to_atom(atom_to_list(Fun) ++ "_pred"),
          logger:debug("Checking ~p:~p(~p) for selection.~n", [Mod, PredFun, Args]),
          case erlang:function_exported(Mod, PredFun, length(Args)) of
            false ->
              logger:debug("No such predicate function ~p:~p(~p).~n", [Mod, PredFun, Args]),
              process_native_request(ActionTerm, Request, Priority, Q2, State);
            true ->
              case apply(Mod, PredFun, Args) of
                false ->
                  logger:debug("Predicate ~p:~p(~p) returned false.~n", [Mod, PredFun, Args]),
                  process_module(ActionTerm, OtherSpecs, Request, Priority, Q2, State);
                true ->
                  logger:debug("Predicate ~p:~p(~p) returned true.~n", [Mod, PredFun, Args]),
                  process_native_request(ActionTerm, Request, Priority, Q2, State)
              end
          end
      end;
    ValidPid when is_pid(ValidPid) ->
      logger:debug("Found external pid ~p~n", [ValidPid]),
      process_external_request(ValidPid, Request, Priority, Q2, State)
  end.

close_if_cast(ActionTerm, Request) ->
  case ActionTerm of
    {cast, _Mod, _Fun, _Args} ->
      Sock = Request#request.sock,
      gen_tcp:send(Sock, term_to_binary({noreply})),
      ok = gen_tcp:close(Sock),
      logger:debug("Closed cast.~n", []);
    _Any ->
      ok
  end.

finish(Priority, Q2, State) ->
  case Priority of
    hq -> State#state{hq = Q2};
    lq -> State#state{lq = Q2}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Native

process_native_request(ActionTerm, Request, Priority, Q2, State) ->
  Count = State#state.count,
  State2 = State#state{count = Count + 1},
  logger:debug("Count = ~p~n", [Count + 1]),
  Log = Request#request.log,
  Log2 = Log#log{type = native, tprocess = erlang:now()},
  Request2 = Request#request{log = Log2},
  spawn(fun() -> ernie_native:process(ActionTerm, Request2) end),
  finish(Priority, Q2, State2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% External

process_external_request(Pid, Request, Priority, Q2, State) ->
  Count = State#state.count,
  State2 = State#state{count = Count + 1},
  logger:debug("Count = ~p~n", [Count + 1]),
  case asset_pool:lease(Pid) of
    {ok, Asset} ->
      logger:debug("Leased asset for pool ~p~n", [Pid]),
      Log = Request#request.log,
      Log2 = Log#log{type = external, tprocess = erlang:now()},
      Request2 = Request#request{log = Log2},
      spawn(fun() -> process_now(Pid, Request2, Asset) end),
      finish(Priority, Q2, State2);
    empty ->
      State
  end.

process_now(Pid, Request, Asset) ->
  try unsafe_process_now(Request, Asset) of
    _AnyResponse ->
      Log = Request#request.log,
      Log2 = Log#log{tdone = erlang:now()},
      Request2 = Request#request{log = Log2},
      ernie_access_logger:acc(Request2)
  catch
    AnyClass:AnyError ->
      Log = Request#request.log,
      Log2 = Log#log{tdone = erlang:now()},
      Request2 = Request#request{log = Log2},
      ernie_access_logger:err(Request2, "External process error ~w: ~w", [AnyClass, AnyError])
  after
    asset_pool:return(Pid, Asset),
    ernie_server:fin(),
    ernie_server:kick(),
    logger:debug("Returned asset ~p~n", [Asset]),
    gen_tcp:close(Request#request.sock),
    logger:debug("Closed socket ~p~n", [Request#request.sock])
  end.

unsafe_process_now(Request, Asset) ->
  BinaryTerm = Request#request.action,
  Term = binary_to_term(BinaryTerm),
  case Term of
    {call, Mod, Fun, Args} ->
      logger:debug("Calling ~p:~p(~p)~n", [Mod, Fun, Args]),
      Sock = Request#request.sock,
      {asset, Port, Token} = Asset,
      logger:debug("Asset: ~p ~p~n", [Port, Token]),
      {ok, Data} = port_wrapper:rpc(Port, BinaryTerm),
      ok = gen_tcp:send(Sock, Data);
    {cast, Mod, Fun, Args} ->
      logger:debug("Casting ~p:~p(~p)~n", [Mod, Fun, Args]),
      {asset, Port, Token} = Asset,
      logger:debug("Asset: ~p ~p~n", [Port, Token]),
      {ok, _Data} = port_wrapper:rpc(Port, BinaryTerm)
  end.