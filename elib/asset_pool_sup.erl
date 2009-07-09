-module(asset_pool_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, Handler} = application:get_env(ernie_server_app, handler),
  io:format("Using handler ~p~n", [Handler]),
  {ok, {{one_for_one, 1, 60},
    [{asset_pool, {asset_pool, start_link, [[2, Handler]]},
    permanent, brutal_kill, worker, [asset_pool]}]}}.