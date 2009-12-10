-module(nat).
-export([add/2, fib/1]).

add(A, B) ->
  A + B.

fib(0) -> 1;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N - 1) + fib(N - 2).