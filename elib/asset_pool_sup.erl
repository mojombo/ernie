-module(asset_pool_sup).
-behaviour(supervisor).
-export([start_link/2, init/1]).

start_link(Handler, Number) ->
  supervisor:start_link(?MODULE, [Handler, Number]).

init([Handler, Number]) ->
  io:format("Using handler ~p~n", [Handler]),
  io:format("Using ~p handler instances~n", [Number]),
  {ok, {{one_for_one, 1, 60},
    [{asset_pool, {asset_pool, start_link, [Handler, Number]},
    permanent, brutal_kill, worker, [asset_pool]}]}}.