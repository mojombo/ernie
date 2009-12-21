%% The MIT License
%% 
%% Copyright (c) 2009 Tom Preston-Werner <tom@mojombo.com>
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% Erlang BERT encoder/decoder. See http://bert-rpc.org for full spec.
%%
%% Watch and contribute to this module at http://github.com/mojombo/bert.erl.
%%
%% This module is Semantic Versioning (http://semver.org) compliant.
%%
%% The following types can be automatically encoded and decoded.
%% See http://www.erlang.org/eeps/eep-0008.html for type definitions.
%%
%%   integer() -> BERT integer
%%   float()   -> BERT float
%%   atom()    -> BERT atom
%%   tuple()   -> BERT tuple
%%   list()    -> BERT list or BERT bytelist
%%   string()  -> BERT list or BERT bytelist (you probably want binary)
%%   binary()  -> BERT binary
%%   []        -> BERT nil (complex)
%%   bool()    -> BERT boolean (complex)
%%   dict()    -> BERT dict (complex)
%%
%% Because times and regular expressions types cannot be automatically
%% detected, you must encode and decode those types manually.
%%
%% To encode Erlang terms to BERT binaries, use:
%%
%%     encode(term()) -> binary().
%%
%% To decode BERT binaries to Erlang terms, use:
%%
%%     decode(binary()) -> term().
%%
%% Examples
%%
%%     % Encode a variety of literal Erlang terms:
%%     bert:encode([42, 3.14, banana, {xy, 5, 10}, <<"robot">>, true, false]).
%%     % -> <<131,108,0,0,0,7,97,42,99,51,46,49,52,48,48,48,48,48,48,...>>
%%
%%     % Encode an Erlang dict() record:
%%     D0 = dict:new().
%%     D1 = dict:store(apple, red, D0).
%%     bert:encode(D1).
%%     % -> <<131,104,9,100,0,4,100,105,99,116,97,0,97,16,97,16,97,8,...>>
%%
%%     % Decode a BERT binary:
%%     bert:decode(<<131,108,0,0,0,7,97,42,99,51,46,49,52,...>>).
%%     % -> [42, 3.14, banana, {xy, 5, 10}, <<"robot">>, true, false]

-module(bert). %% Version 1.0.0

-author("Tom Preston-Werner").

-export([encode/1, decode/1]).

%%---------------------------------------------------------------------------
%% Public API

-spec encode(term()) -> binary().

encode(Term) ->
  term_to_binary(encode_term(Term)).

-spec decode(binary()) -> term().

decode(Bin) ->
  decode_term(binary_to_term(Bin)).

%%---------------------------------------------------------------------------
%% Encode

-spec encode_term(term()) -> term().

encode_term(Term) ->
  case Term of
    [] -> {bert, nil};
    true -> {bert, true};
    false -> {bert, false};
    Dict when is_record(Term, dict, 8) -> {bert, dict, dict:to_list(Dict)};
    List when is_list(Term) -> lists:map((fun encode_term/1), List);
    _Else -> Term
  end.

%%---------------------------------------------------------------------------
%% Decode

-spec decode_term(term()) -> term().

decode_term(Term) ->
  case Term of
    {bert, nil} -> [];
    {bert, true} -> true;
    {bert, false} -> false;
    {bert, dict, Dict} -> dict:from_list(Dict);
    List when is_list(Term) -> lists:map((fun decode_term/1), List);
    _Else -> Term
  end.