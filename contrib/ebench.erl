% erlc *.erl && erl ebench.beam -run ebench start 10000 20 ext add

-module(ebench).
-export([start/1]).

start([Ni, Ci, Modi, Funi]) ->
  Nt = list_to_integer(Ni),
  C = list_to_integer(Ci),
  Mod = list_to_atom(Modi),
  Fun = list_to_atom(Funi),
  N = round(Nt / C),
  T0 = erlang:now(),
  Waiter = spawn(fun() -> wait(T0, N * C) end),
  spawner(Waiter, N, C, Mod, Fun).

spawner(_Waiter, _N, 0, _Mod, _Fun) ->
  ok;
spawner(Waiter, N, C, Mod, Fun) ->
  spawn(fun() -> loop(Waiter, N, Mod, Fun) end),
  spawner(Waiter, N, C - 1, Mod, Fun).

% X is the total number of responses to wait for
wait(T0, XTotal, 0) ->
  T1 = erlang:now(),
  Diff = timer:now_diff(T1, T0),
  Mean = Diff / XTotal,
  io:format("~p requests completed in ~.2fs~n", [XTotal, Diff / 1000000]),
  io:format("Mean request time: ~.2fms (~.2f r/s)~n", [Mean / 1000, XTotal / (Diff / 1000000)]),
  init:stop();
wait(T0, XTotal, X) ->
  receive
    done -> wait(T0, XTotal, X - 1)
  end.

wait(T0, X) ->
  wait(T0, X, X).

loop(_Waiter, 0, _Mod, _Fun) ->
  ok;
loop(Waiter, N, Mod, Fun) ->
  hit(Waiter, Mod, Fun),
  loop(Waiter, N - 1, Mod, Fun).

hit(Waiter, Mod, Fun) ->
  % io:format("outgoing!~n", []),
  Host = "localhost",
  {ok, Sock} = gen_tcp:connect(Host, 8000, [binary, {packet, 4}]),
  % Info = term_to_binary({info, priority, [low]}),
  % ok = gen_tcp:send(Sock, Info),
  Request = term_to_binary({call, Mod, Fun, args(Fun)}),
  ok = gen_tcp:send(Sock, Request),
  receive
    {tcp, _Port, Reply} ->
      % io:format("~p~n", [Reply]),
      Res = res(Fun),
      {reply, Res} = binary_to_term(Reply),
      Waiter ! done,
      ok;
    Any ->
      io:format("Unexpected message: ~p~n", [Any]),
      Waiter ! done,
      ok
  end,
  ok = gen_tcp:close(Sock).

args(add) -> [1, 2];
args(fib) -> [20].

res(add) -> 3;
res(fib) -> 10946.