-module(asset_pool).
-behaviour(gen_server).

%% api
-export([start_link/1, start/1, lease_asset/0, return_asset/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {assets = undefined,
                handler = undefined}).

%%====================================================================
%% API
%%====================================================================

start_link(Args) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

start(Args) ->
  gen_server:start({global, ?MODULE}, ?MODULE, Args, []).

lease_asset() ->
  gen_server:call({global, ?MODULE}, {lease_asset}).

return_asset(Asset) ->
  gen_server:call({global, ?MODULE}, {return_asset, Asset}).

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
init([Count, Handler]) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("~p starting~n", [?MODULE]),
  Assets = start_handlers(Count, Handler),
  {ok, #state{assets = Assets, handler = Handler}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({lease_asset}, _From, State) ->
  {{value, Asset}, Assets2} = queue:out(State#state.assets),
  {reply, Asset, State#state{assets = Assets2}};
handle_call({return_asset, Asset}, _From, State) ->
  Assets2 = queue:in(Asset, State#state.assets),
  {reply, ok, State#state{assets = Assets2}};
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
  Asset = port_wrapper:wrap_link("ruby " ++ Handler),
  Assets = queue:in(Asset, State#state.assets),
  {noreply, State#state{assets = Assets}};
handle_info(Msg, State) ->
  error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
  {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal
%%====================================================================

start_handlers(Count, Handler) ->
  start_handlers(queue:new(), Count, Handler).

start_handlers(Assets, 0, _Handler) ->
  Assets;
start_handlers(Assets, Count, Handler) ->
  Asset = port_wrapper:wrap_link("ruby " ++ Handler),
  Assets2 = queue:in(Asset, Assets),
  start_handlers(Assets2, Count - 1, Handler).