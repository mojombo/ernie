-module(ernie_native).
-export([process/2]).
-include_lib("ernie.hrl").

process(ActionTerm, Request) ->
  {_Type, Mod, Fun, Args} = ActionTerm,
  Sock = Request#request.sock,
  logger:debug("Calling ~p:~p(~p)~n", [Mod, Fun, Args]),
  try apply(Mod, Fun, Args) of
    Result ->
      logger:debug("Result was ~p~n", [Result]),
      Data = bert:encode({reply, Result}),
      gen_tcp:send(Sock, Data)
  catch
    error:Error ->
      BError = list_to_binary(io_lib:format("~p", [Error])),
      Trace = erlang:get_stacktrace(),
      BTrace = lists:map(fun(X) -> list_to_binary(io_lib:format("~p", [X])) end, Trace),
      Data = term_to_binary({error, [user, 0, <<"RuntimeError">>, BError, BTrace]}),
      gen_tcp:send(Sock, Data)
  end,
  ok = gen_tcp:close(Sock),
  Log = Request#request.log,
  Log2 = Log#log{tdone = erlang:now()},
  Request2 = Request#request{log = Log2},
  ernie_access_logger:log(Request2).