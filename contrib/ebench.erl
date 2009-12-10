% erlc *.erl && erl ebench.beam -run ebench start 10000 20

-module(ebench).
-export([start/1]).

start([Ni, Ci]) ->
  Nt = list_to_integer(Ni),
  C = list_to_integer(Ci),
  N = round(Nt / C),
  T0 = erlang:now(),
  Waiter = spawn(fun() -> wait(T0, N * C) end),
  spawner(Waiter, N, C).

spawner(_Waiter, _N, 0) ->
  ok;
spawner(Waiter, N, C) ->
  spawn(fun() -> loop(Waiter, N) end),
  spawner(Waiter, N, C - 1).

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

loop(_Waiter, 0) ->
  ok;
loop(Waiter, N) ->
  hit(Waiter),
  loop(Waiter, N - 1).

hit(Waiter) ->
  % io:format("outgoing!~n", []),
  Host = "localhost",
  {ok, Sock} = gen_tcp:connect(Host, 8000, [binary, {packet, 4}]),
  Request = term_to_binary({call, nat, add, [1, 2]}),
  ok = gen_tcp:send(Sock, Request),
  receive
    {tcp, _Port, Reply} ->
      % io:format("~p~n", [Reply]),
      {reply, 3} = binary_to_term(Reply),
      Waiter ! done,
      ok;
    Any ->
      io:format("Unexpected message: ~p~n", [Any]),
      Waiter ! done,
      ok
  end,
  ok = gen_tcp:close(Sock).