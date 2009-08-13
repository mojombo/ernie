-module(ernie_server_app).
-behaviour(application).

-export([boot/0, start/2, stop/1]).

boot() ->
  application:start(ernie_server_app).

start(_Type, _Args) ->
  logger_sup:start_link(),
  ernie_server_sup:start_link(),
  asset_pool_sup:start_link().

stop(_State) ->
  ok.
