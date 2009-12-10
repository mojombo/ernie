-module(native).
-export([process/2]).

-record(request, {sock = undefined,
                  info = undefined,
                  action = undefined}).

process(ActionTerm, Request) ->
  {_Type, Mod, Fun, Args} = ActionTerm,
  Sock = Request#request.sock,
  logger:debug("Calling ~p:~p(~p)~n", [Mod, Fun, Args]),
  Result = apply(Mod, Fun, Args),
  logger:debug("Result was ~p~n", [Result]),
  Data = term_to_binary({reply, Result}),
  gen_tcp:send(Sock, Data),
  ok = gen_tcp:close(Sock).