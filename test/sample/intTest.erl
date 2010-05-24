-module(intTest).

-export([zeronary/0, unary/1, binary/2, ternary/3, big/1, set_state/1, get_state/0, connect_nodes/0, sleep/1]).

connect_nodes() ->
	net_adm:ping('ernie0@127.0.0.1').

zeronary() ->
	foo.

unary(A) ->
	A.
	
binary(A,B) ->
	A + B.

ternary(A,B,C) ->
	A + B + C.
	
big(A) ->
	string:copies("a", A).

sleep(Time) ->
	receive after Time ->
		ok
	end.

get_state() ->
	case catch global:send(test_saved_state, {get_state, self()}) of
		{'EXIT',{badarg, _}} ->
			{error, no_record};
		_ ->
			receive
				{ok, State} ->
					State
				after 1000 ->
					{error, timeout}
			end
	end.
	
set_state(State) ->
	spawn(fun() -> wrapper(State) end),
	ok.

wrapper(State) ->
	case global:register_name(test_saved_state, self()) of
		no ->
			global:send(test_saved_state, {set_state, State});
		yes ->
			recv(State)
	end.

recv(State) ->
	receive
		{set_state, NewState} ->
			recv(NewState);
		{get_state, Pid} ->
			Pid ! {ok, State},
			recv(State)
	end.
