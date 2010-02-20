-module(ernie_access_logger).
-behaviour(gen_server).

%% api
-export([start_link/1, start/1, log/1, reopen/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("ernie.hrl").

-record(lstate, {access_file_name = undefined,
                 access_file = undefined}).

%%====================================================================
%% API
%%====================================================================

start_link(Args) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

start(Args) ->
  gen_server:start({global, ?MODULE}, ?MODULE, Args, []).

log(Request) ->
  gen_server:cast({global, ?MODULE}, {log, Request}).

reopen() ->
  gen_server:cast({global, ?MODULE}, reopen).

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
init([undefined]) ->
  error_logger:info_msg("~p starting~n", [?MODULE]),
  {ok, #lstate{}};
init([AccessFileName]) ->
  error_logger:info_msg("~p starting~n", [?MODULE]),
  {ok, AccessFile} = file:open(AccessFileName, [append]),
  {ok, _T} = timer:apply_interval(10000, ernie_access_logger, reopen, []),
  {ok, #lstate{access_file_name = AccessFileName,
               access_file = AccessFile}}.

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
handle_cast({log, Request}, State) ->
  case State#lstate.access_file_name of
    undefined -> ok;
    _AccessFilename -> log(Request, State)
  end,
  {noreply, State};
handle_cast(reopen, State) ->
  case State#lstate.access_file_name of
    undefined ->
      {noreply, State};
    AccessFileName ->
      ok = file:close(State#lstate.access_file),
      {ok, AccessFile} = file:open(AccessFileName, [append]),
      {noreply, State#lstate{access_file = AccessFile}}
  end;
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Msg, State) ->
  error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
  {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal
%%====================================================================

log(Request, State) ->
  Log = Request#request.log,
  TAccept = time_tuple_to_iso_8601_date(Log#log.taccept),
  D1 = time_difference_in_seconds(Log#log.taccept, Log#log.tprocess),
  D2 = time_difference_in_seconds(Log#log.tprocess, Log#log.tdone),
  Type = Log#log.type,
  HQ = Log#log.hq,
  LQ = Log#log.lq,
  Prio = Request#request.priority,
  Action = lists:flatten(io_lib:fwrite("~200.0.0p", [binary_to_term(Request#request.action)])),
  case string:len(Action) > 200 of
    true -> Trunc = [string:sub_string(Action, 200), "..."];
    false -> Trunc = Action
  end,
  Args = [TAccept, D1, D2, HQ, LQ, Type, Prio, Trunc],
  Line = io_lib:fwrite("[~s] ~f ~f - ~B ~B ~p ~p ~s~n", Args),
  file:write(State#lstate.access_file, Line).

time_tuple_to_iso_8601_date(TimeTuple) ->
  {{YY, MM, DD}, {H, M, S}} = calendar:now_to_local_time(TimeTuple),
  {_MegaSecs, _Secs, MicroSecs} = TimeTuple,
  Args = [YY, MM, DD, H, M, S, MicroSecs],
  io_lib:fwrite("~4B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B.~-6.10.0B", Args).

time_difference_in_seconds(T1, T2) ->
  {_, _, MS1} = T1,
  {_, _, MS2} = T2,
  S1 = calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(T1)),
  S2 = calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(T2)),
  F1 = S1 + (MS1 / 1000000),
  F2 = S2 + (MS2 / 1000000),
  F2 - F1.