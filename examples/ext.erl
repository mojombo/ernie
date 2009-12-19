-module(ext).
-export([shadow_pred/1, shadow/1]).

shadow_pred(X) ->
  X > 10.

shadow(_X) ->
  <<"erlang">>.