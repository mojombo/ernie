-module(logger).
-behaviour(gen_server).

%% api
-export([start_link/1, start/1, set_log_level/1, debug/2, info/2, warn/2, error/2, fatal/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {log_level = undefined}).

%%====================================================================
%% API
%%====================================================================

start_link(Args) ->
  gen_server:start_link({global, {node(),?MODULE}}, ?MODULE, Args, []).

start(Args) ->
  gen_server:start({global, {node(),?MODULE}}, ?MODULE, Args, []).

set_log_level(Level) ->
  gen_server:call({global, {node(),?MODULE}}, {set_log_level, Level}).

debug(Msg, Args) ->
  gen_server:cast({global, {node(),?MODULE}}, {debug, Msg, Args}).

info(Msg, Args) ->
  gen_server:cast({global, {node(),?MODULE}}, {info, Msg, Args}).

warn(Msg, Args) ->
  gen_server:cast({global, {node(),?MODULE}}, {warn, Msg, Args}).

error(Msg, Args) ->
  gen_server:cast({global, {node(),?MODULE}}, {error, Msg, Args}).

fatal(Msg, Args) ->
  gen_server:cast({global, {node(),?MODULE}}, {fatal, Msg, Args}).

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
init([LogLevel]) ->
  error_logger:info_msg("~p starting~n", [?MODULE]),
  {ok, #state{log_level = LogLevel}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({set_log_level, Level}, _From, State) ->
  {reply, ok, State#state{log_level = Level}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({debug, Msg, Args}, State) ->
  log(State#state.log_level, 4, Msg, Args),
  {noreply, State};
handle_cast({info, Msg, Args}, State) ->
  log(State#state.log_level, 3, Msg, Args),
  {noreply, State};
handle_cast({warn, Msg, Args}, State) ->
  log(State#state.log_level, 2, Msg, Args),
  {noreply, State};
handle_cast({error, Msg, Args}, State) ->
  log(State#state.log_level, 1, Msg, Args),
  {noreply, State};
handle_cast({fatal, Msg, Args}, State) ->
  log(State#state.log_level, 0, Msg, Args),
  {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(Msg, State) ->
  error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
  {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal
%%====================================================================

log(SystemLogLevel, MessageLogLevel, Message, Args) ->
  case SystemLogLevel >= MessageLogLevel of
    false -> ok;
    true -> io:format(Message, Args)
  end.