-module(ernie_admin).
-export([process/4]).
-include_lib("ernie.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process entry point

process(Sock, reload_handlers, _Args, State) ->
  spawn(fun() -> process_reload_assets(Sock, State) end),
  State;
process(Sock, halt, _Args, State) ->
  process_halt(Sock, State),
  State#state{listen = false};
process(Sock, stats, _Args, State) ->
  spawn(fun() -> process_stats(Sock, State) end),
  State;
process(Sock, _Fun, _Args, State) ->
  gen_tcp:send(Sock, term_to_binary({reply, <<"Admin function not supported.">>})),
  ok = gen_tcp:close(Sock),
  State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reload handlers

process_reload_assets(Sock, State) ->
  lists:map((fun reload/1), State#state.map),
  gen_tcp:send(Sock, term_to_binary({reply, <<"Handlers reloaded.">>})),
  ok = gen_tcp:close(Sock).

reload({_Mod, native}) ->
  ok;
reload({_Mod, Pid}) ->
  asset_pool:reload_assets(Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Halt

process_halt(Sock, State) ->
  gen_tcp:send(Sock, term_to_binary({reply, <<"Halting.">>})),
  ok = gen_tcp:close(Sock),
  gen_tcp:close(State#state.lsock),
  case State#state.count =:= State#state.zcount of
    true -> halt();
    false -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stats

process_stats(Sock, State) ->
  CountString = stat(count, State),
  ZCountString = stat(zcount, State),
  IdleWorkersString = stat(idle, State),
  QueueLengthString = stat(queue, State),
  StatString = list_to_binary([CountString, ZCountString, IdleWorkersString, QueueLengthString]),
  Data = term_to_binary({reply, StatString}),
  gen_tcp:send(Sock, Data),
  ok = gen_tcp:close(Sock).

stat(count, State) ->
  Count = State#state.count,
  list_to_binary([<<"connections.total=">>, integer_to_list(Count), <<"\n">>]);
stat(zcount, State) ->
  ZCount = State#state.zcount,
  list_to_binary([<<"connections.completed=">>, integer_to_list(ZCount), <<"\n">>]);
stat(idle, State) ->
  IdleMap = lists:map((fun idle/1), State#state.map),
  list_to_binary(IdleMap);
stat(queue, State) ->
  HighQueueLength = queue:len(State#state.hq),
  LowQueueLength = queue:len(State#state.lq),
  list_to_binary([<<"queue.high=">>, integer_to_list(HighQueueLength), <<"\n">>,
                  <<"queue.low=">>, integer_to_list(LowQueueLength), <<"\n">>]).

idle({Mod, native}) ->
  list_to_binary([<<"workers.idle.">>, atom_to_list(Mod), <<"=native\n">>]);
idle({Mod, Pid}) ->
  IdleCount = integer_to_list(asset_pool:idle_worker_count(Pid)),
  list_to_binary([<<"workers.idle.">>, atom_to_list(Mod), <<"=">>, IdleCount, <<"\n">>]).
