-module(ernie_config).
-export([load/1]).

load(ConfigFile) ->
  {ok, Configs} = file:consult(ConfigFile),
  Configs2 = lists:map((fun load_single/1), Configs),
  {ok, Configs2}.

load_single(Config) ->
  case proplists:get_value(type, Config) of
    native ->
      verify(native, Config),
      CodePaths = proplists:get_value(codepaths, Config),
      lists:map((fun code:add_patha/1), CodePaths),
      [{id, native} | Config];
    extern ->
      verify(extern, Config),
      Handler = proplists:get_value(command, Config),
      Number = proplists:get_value(count, Config),
      {ok, SupPid} = asset_pool_sup:start_link(Handler, Number),
      [{_Id, ChildPid, _Type, _Modules}] = supervisor:which_children(SupPid),
      [{id, ChildPid} | Config]
  end.

verify(native, _Config) ->
  ok;
verify(extern, _Config) ->
  ok.