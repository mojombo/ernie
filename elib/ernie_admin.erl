-module(ernie_admin).
-export([process/4]).

-record(state, {lsock = undefined,
                pending = queue:new(),
                count = 0,
                map = undefined}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process entry point

process(Sock, reload_handlers, _Args, State) ->
  spawn(fun() -> process_reload_assets(Sock, State) end),
  State;
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
% Stats

process_stats(Sock, State) ->
  CountString = stat(count, State),
  IdleWorkersString = stat(idle, State),
  QueueLengthString = stat(queue, State),
  StatString = list_to_binary([CountString, IdleWorkersString, QueueLengthString]),
  Data = term_to_binary({reply, StatString}),
  gen_tcp:send(Sock, Data),
  ok = gen_tcp:close(Sock).

stat(count, State) ->
  Count = State#state.count,
  list_to_binary([<<"connections.total=">>, integer_to_list(Count), <<"\n">>]);
stat(idle, State) ->
  IdleMap = lists:map((fun idle/1), State#state.map),
  list_to_binary(IdleMap);
stat(queue, State) ->
  QueueLength = queue:len(State#state.pending),
  list_to_binary([<<"connections.pending=">>, integer_to_list(QueueLength), <<"\n">>]).

idle({Mod, native}) ->
  list_to_binary([<<"workers.idle.">>, atom_to_list(Mod), <<"=native\n">>]);
idle({Mod, Pid}) ->
  IdleCount = integer_to_list(asset_pool:idle_worker_count(Pid)),
  list_to_binary([<<"workers.idle.">>, atom_to_list(Mod), <<"=">>, IdleCount, <<"\n">>]).
