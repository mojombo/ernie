-module(ernie_server_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, Port} = application:get_env(ernie_server_app, port),
  io:format("Using port ~p~n", [Port]),
  {ok, Handler} = application:get_env(ernie_server_app, handler),
  io:format("Using handler ~p~n", [Handler]),
  case application:get_env(ernie_server_app, pidfile) of
    {ok, Location} ->
      Pid = os:getpid(),
      ok = file:write_file(Location, list_to_binary(Pid));
    undefined -> ok
  end,
  {ok, {{one_for_one, 1, 60},
    [{ernie_server, {ernie_server, start_link, [[Port, Handler]]},
    permanent, brutal_kill, worker, [ernie_server]}]}}.