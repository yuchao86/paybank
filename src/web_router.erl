%%%-------------------------------------------------------------------
%%% File    : web_router.erl
%%% Author  : asceth <machinist@asceth.com>
%%% Description : Routes requests from mochiweb to whatever apps
%%%                are hooked into it.
%%%
%%% Created :  7 Sep 2008 by asceth <machinist@asceth.com>
%%%-------------------------------------------------------------------
-module(web_router).

%% External API
-export([load_bindings/2]).
-export([key/1, id/2]).

-include("logger.hrl").
-include_lib("eunit/include/eunit.hrl").


-spec(load_bindings/2 :: (list(), list()) -> ok).
load_bindings(_WebExchangeName, []) ->
  ok;
load_bindings(WebExchangeName, [Head|Tail]) ->
  {Key, Callback} = Head,
  web_router_exchange:add_binding(WebExchangeName, Callback, Key),
  load_bindings(WebExchangeName, Tail).

-spec(id/2 :: (list(), integer()) -> integer() | list()).
id([], _Num) ->
  undefined;
id([Head|_Tail], 1) ->
  list_to_integer(Head);
id([_Head|Tail], Num) ->
  id(Tail, Num-1).

-spec(key/1 :: (list()) -> binary()).
key([]) ->
  <<>>;
key(List) ->
  key(List, []).

key([], Acc) ->
  list_to_binary(lists:flatten(lists:reverse(Acc)));
key([H|T], []) ->
  key(T, [to_list(H)]);
key([""|T], Acc) ->
  key(T, Acc);
key([H|T], Acc) ->
  Str = "." ++ to_list(H),
  key(T, [Str|Acc]).

to_list(Term) when is_list(Term) ->
  Term;
to_list(Term) when is_atom(Term) ->
  atom_to_list(Term);
to_list(Term) when is_binary(Term) ->
  binary_to_list(Term);
to_list(Term) when is_number(Term) ->
  integer_to_list(Term);
to_list(Term) when is_float(Term) ->
  float_to_list(Term).



%%--------------------------------------------------------------------
%%% Tests
%%--------------------------------------------------------------------
key_test_() ->
  [?_assertEqual(key(["get", "request"]), <<"get.request">>),
   ?_assertEqual(key(["get", request]), <<"get.request">>),
   ?_assertEqual(key(["get", "request", "global"]), <<"get.request.global">>),
   ?_assertEqual(key(["get", "request", global]), <<"get.request.global">>),
   ?_assertEqual(key(["get", "request", "users", 12]), <<"get.request.users.12">>),
   ?_assertEqual(key([get, request|["users", 12]]), <<"get.request.users.12">>),
   ?_assertEqual(key([get, request, "", "users"]), <<"get.request.users">>)
  ].

