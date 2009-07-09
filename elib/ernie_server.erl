-module(ernie_server).
-behaviour(gen_server).

%% api
-export([start_link/1, start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lsock = undefined,
                handler = undefined,
                ducky = undefined}).

%%====================================================================
%% API
%%====================================================================

start_link(Args) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

start(Args) ->
  gen_server:start({global, ?MODULE}, ?MODULE, Args, []).

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
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({'EXIT', _Pid, _Error}, State) ->
  error_logger:error_msg("Port closed, restarting port...~n", []),
  Handler = State#state.handler,
  Ducky = port_wrapper:wrap_link("ruby " ++ Handler),
  {noreply, State#state{ducky = Ducky}};
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
  Res = gen_tcp:listen(Port, [binary, {packet, 4}, {active, false}]),
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
  spawn(fun() -> handle_method(Sock) end),
  loop(LSock).

handle_method(Sock) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, BinaryTerm} ->
      io:format(".", []),
      Asset = asset_pool:lease_asset(),
      % error_logger:info_msg("From Internet: ~p~n", [BinaryTerm]),
      {ok, Data} = port_wrapper:rpc(Asset, BinaryTerm),
      % error_logger:info_msg("From Port: ~p~n", [Data]),
      asset_pool:return_asset(Asset),
      gen_tcp:send(Sock, Data),
      ok = gen_tcp:close(Sock);
    {error, closed} ->
      ok = gen_tcp:close(Sock)
  end.