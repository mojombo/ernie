-module(ernie_server).
-behaviour(gen_server).

%% api
-export([start_link/1, start/1, process/1, asset_freed/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock = undefined,
                pending = queue:new()}).

%%====================================================================
%% API
%%====================================================================

start_link(Args) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

start(Args) ->
  gen_server:start({global, ?MODULE}, ?MODULE, Args, []).

process(Sock) ->
  gen_server:cast({global, ?MODULE}, {process, Sock}).

asset_freed() ->
  gen_server:cast({global, ?MODULE}, {asset_freed}).

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
init([Port]) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("~p starting~n", [?MODULE]),
  {ok, LSock} = try_listen(Port, 500),
  spawn(fun() -> loop(LSock) end),
  {ok, #state{lsock = LSock}}.

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
  case queue:is_empty(State#state.pending) of
    false ->
      Pending2 = queue:in(Sock, State#state.pending),
      io:format("Q", []),
      {noreply, State#state{pending = Pending2}};
    true ->
      State2 = try_process_now(Sock, State),
      {noreply, State2}
    end;
handle_cast({asset_freed}, State) ->
  case queue:is_empty(State#state.pending) of
    false ->
      case asset_pool:lease() of
        {ok, Asset} ->
          {{value, Sock}, Pending2} = queue:out(State#state.pending),
          io:format("d", []),
          spawn(fun() -> process_now(Sock, Asset) end),
          {noreply, State#state{pending = Pending2}};
        empty ->
          io:format(".", []),
          {noreply, State}
      end;
    true ->
      {noreply, State}
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

try_listen(Port, 0) ->
  error_logger:error_msg("Could not listen on port ~p~n", [Port]),
  {error, "Could not listen on port"};
try_listen(Port, Times) ->
  Res = gen_tcp:listen(Port, [binary, {packet, 4}, {active, false}, {reuseaddr, true}]),
  case Res of
    {ok, LSock} ->
      error_logger:info_msg("Listening on port ~p~n", [Port]),
      {ok, LSock};
    {error, Reason} ->
      error_logger:info_msg("Could not listen on port ~p: ~p~n", [Port, Reason]),
      timer:sleep(5000),
      try_listen(Port, Times - 1)
  end.

loop(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  ernie_server:process(Sock),
  loop(LSock).

try_process_now(Sock, State) ->
  case asset_pool:lease() of
    {ok, Asset} ->
      io:format("i", []),
      spawn(fun() -> process_now(Sock, Asset) end),
      State;
    empty ->
      io:format("q", []),
      Pending2 = queue:in(Sock, State#state.pending),
      State#state{pending = Pending2}
  end.

process_now(Sock, Asset) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, BinaryTerm} ->
      % io:format(".", []),
      % error_logger:info_msg("From Internet: ~p~n", [BinaryTerm]),
      {asset, Port, _Token} = Asset,
      {ok, Data} = port_wrapper:rpc(Port, BinaryTerm),
      % error_logger:info_msg("From Port: ~p~n", [Data]),
      asset_pool:return(Asset),
      ernie_server:asset_freed(),
      gen_tcp:send(Sock, Data),
      ok = gen_tcp:close(Sock);
    {error, closed} ->
      asset_pool:return(Asset),
      ernie_server:asset_freed(),
      io:format("c", []),
      ok = gen_tcp:close(Sock)
  end.