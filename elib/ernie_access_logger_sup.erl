-module(ernie_access_logger_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(AccessLog) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [AccessLog]).

init([AccessLog]) ->
  case AccessLog of
    undefined -> io:format("No access log~n", []);
    Any -> io:format("Using access log ~p~n", [Any])
  end,
  {ok, {{one_for_one, 1, 60},
    [{ernie_access_logger, {ernie_access_logger, start_link, [[AccessLog]]},
    permanent, brutal_kill, worker, [ernie_access_logger]}]}}.