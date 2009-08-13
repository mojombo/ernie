-module(logger_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, LogLevel} = application:get_env(ernie_server_app, log_level),
  io:format("Using log level ~p~n", [LogLevel]),
  {ok, {{one_for_one, 1, 60},
    [{logger, {logger, start_link, [[LogLevel]]},
    permanent, brutal_kill, worker, [logger]}]}}.