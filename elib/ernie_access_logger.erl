-module(ernie_access_logger).
-behaviour(gen_server).

%% api
-export([start_link/1, start/1, acc/1, err/3, reopen/0]).

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

acc(Request) ->
  gen_server:cast({global, ?MODULE}, {acc, Request}).

err(Request, Msg, Args) ->
  gen_server:cast({global, ?MODULE}, {err, Request, Msg, Args}).

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
  case file:open(AccessFileName, [append]) of
    {ok, AccessFile} ->
      {ok, _T} = timer:apply_interval(10000, ernie_access_logger, reopen, []),
      {ok, #lstate{access_file_name = AccessFileName,
                   access_file = AccessFile}};
    {error, Error} ->
      error_logger:error_msg("Error opening access log ~p: ~p.~n", [AccessFileName, Error]),
      {ok, #lstate{}}
  end.

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
handle_cast({acc, Request}, State) ->
  case State#lstate.access_file_name of
    undefined -> ok;
    _AccessFilename -> acc(Request, State)
  end,
  {noreply, State};
handle_cast({err, Request, Msg, Args}, State) ->
  case State#lstate.access_file_name of
    undefined -> ok;
    _AccessFilename -> err(Request, Msg, Args, State)
  end,
  {noreply, State};
handle_cast(reopen, State) ->
  case State#lstate.access_file_name of
    undefined ->
      {noreply, State};
    AccessFileName ->
      case file:read_file_info(AccessFileName) of
        {ok, _FileInfo} ->
          {noreply, State};
        {error, enoent} ->
          ok = file:close(State#lstate.access_file),
          {ok, AccessFile} = file:open(AccessFileName, [append]),
          {noreply, State#lstate{access_file = AccessFile}};
        _OtherError ->
          {noreply, #lstate{}}
      end
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

acc(Request, State) ->
  StatString = stat_string(Request),
  ActionString = action_string(Request),
  Line = io_lib:fwrite("ACC ~s - ~s~n", [StatString, ActionString]),
  file:write(State#lstate.access_file, Line).

err(Request, Msg, Args, State) ->
  StatString = stat_string(Request),
  ActionString = action_string(Request),
  ErrString = io_lib:fwrite(Msg, Args),
  Line = io_lib:fwrite("ERR ~s - ~s : ~s~n", [StatString, ErrString, ActionString]),
  file:write(State#lstate.access_file, Line).

stat_string(Request) ->
  Log = Request#request.log,
  TAccept = time_tuple_to_iso_8601_date(Log#log.taccept),
  D1 = time_difference_in_seconds(Log#log.taccept, Log#log.tprocess),
  D2 = time_difference_in_seconds(Log#log.tprocess, Log#log.tdone),
  Type = Log#log.type,
  HQ = Log#log.hq,
  LQ = Log#log.lq,
  Prio = Request#request.priority,
  Args = [TAccept, D1, D2, HQ, LQ, Type, Prio],
  io_lib:fwrite("[~s] ~f ~f - ~B ~B ~3s ~p", Args).

action_string(Request) ->
  TermAction = binary_to_term(Request#request.action),
  RawAction = lists:flatten(io_lib:fwrite("~1000000000.0.0p", [TermAction])),
  case string:len(RawAction) > 150 of
    true ->
      Action = re:replace(RawAction, "\n", "", [global, {return, list}]),
      [string:sub_string(Action, 1, 150), "..."];
    false ->
      RawAction
  end.

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