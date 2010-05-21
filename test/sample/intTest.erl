-module(intTest).

-export([zeronary/0, unary/1, binary/2, ternary/3, big/1]).

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