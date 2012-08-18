%%%-------------------------------------------------------------------
%%% File    : web_exchange.erl
%%% Author  : asceth <machinist@asceth.com>
%%% Description : Routes requests by exchange.key to whatever apps
%%%                are hooked into it.
%%%-------------------------------------------------------------------
%%   The contents of this file are subject to the Mozilla Public License
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%   The Original Code is RabbitMQ.
%%
%%   The Initial Developers of the Original Code are LShift Ltd,
%%   Cohesive Financial Technologies LLC, and Rabbit Technologies Ltd.
%%
%%   Portions created before 22-Nov-2008 00:00:00 GMT by LShift Ltd,
%%   Cohesive Financial Technologies LLC, or Rabbit Technologies Ltd
%%   are Copyright (C) 2007-2008 LShift Ltd, Cohesive Financial
%%   Technologies LLC, and Rabbit Technologies Ltd.
%%
%%   Portions created by LShift Ltd are Copyright (C) 2007-2009 LShift
%%   Ltd. Portions created by Cohesive Financial Technologies LLC are
%%   Copyright (C) 2007-2009 Cohesive Financial Technologies
%%   LLC. Portions created by Rabbit Technologies Ltd are Copyright
%%   (C) 2007-2009 Rabbit Technologies Ltd.
%%
%%   All Rights Reserved.
%%
%%   Contributor(s): asceth <machinist@asceth.com>.
%%

-module(web_router_exchange).
-include_lib("stdlib/include/qlc.hrl").
-include("web_router.hrl").
-include("logger.hrl").


-export([recover/0]).
-export([declare/4]).
-export([lookup/1, lookup_or_die/1]).
-export([info/1, info/2]).
-export([route/3]).
-export([add_binding/3, delete_binding/3]).
-export([delete/2]).
-export([check_type/1, assert_type/2, topic_matches/2]).

%% EXTENDED API
-export([list_exchange_bindings/1]).

-import(mnesia).
-import(lists).
-import(qlc).
-import(regexp).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-type(publish_res() :: {'ok', [pid()]} |
      not_found() | {'error', 'unroutable' | 'not_delivered'}).
-type(bind_res() :: 'ok' |
      {'error', 'queue_not_found' | 'exchange_not_found'}).
-spec(recover/0 :: () -> 'ok').
-spec(declare/4 :: (exchange_name(), exchange_type(), bool(), bool()) -> exchange()).
-spec(check_type/1 :: (binary()) -> atom()).
-spec(assert_type/2 :: (exchange(), atom()) -> 'ok').
-spec(lookup/1 :: (exchange_name()) -> {'ok', exchange()} | not_found()).
-spec(lookup_or_die/1 :: (exchange_name()) -> exchange()).
-spec(list/1 :: (vhost()) -> [exchange()]).
-spec(info/1 :: (exchange()) -> [info()]).
-spec(info/2 :: (exchange(), [info_key()]) -> [info()]).
-spec(info_all/1 :: (vhost()) -> [[info()]]).
-spec(info_all/2 :: (vhost(), [info_key()]) -> [[info()]]).
-spec(route/3 :: (exchange(), routing_key(), decoded_content()) -> [pid()]).
-spec(add_binding/3 ::
      (exchange_name(), queue_name(), routing_key()) ->
         bind_res() | {'error', 'durability_settings_incompatible'}).
-spec(delete_binding/3 ::
      (exchange_name(), queue_name(), routing_key()) ->
         bind_res() | {'error', 'binding_not_found'}).
-spec(topic_matches/2 :: (binary(), binary()) -> bool()).
-spec(delete/2 :: (exchange_name(), bool()) ->
         'ok' | not_found() | {'error', 'in_use'}).
-spec(list_exchange_bindings/1 :: (exchange_name()) ->
         [{queue_name(), routing_key()}]).

-endif.

%%----------------------------------------------------------------------------

-define(INFO_KEYS, [name, type, durable, auto_delete].

recover() ->
  execute_mnesia_transaction(
    fun () ->
        mnesia:foldl(
          fun (Exchange, Acc) ->
              ok = mnesia:write(web_exchange, Exchange, write),
              Acc
          end, ok, web_durable_exchange),
        mnesia:foldl(
          fun (Route, Acc) ->
              {_, ReverseRoute} = route_with_reverse(Route),
              ok = mnesia:write(web_route,
                                Route, write),
              ok = mnesia:write(web_reverse_route,
                                ReverseRoute, write),
              Acc
          end, ok, web_durable_route),
        ok
    end).

declare(ExchangeName, Type, Durable, AutoDelete) ->
  Exchange = #exchange{name = ExchangeName,
                       type = Type,
                       durable = Durable,
                       auto_delete = AutoDelete},
  execute_mnesia_transaction(
    fun () ->
        case mnesia:wread({web_exchange, ExchangeName}) of
          [] -> ok = mnesia:write(web_exchange, Exchange, write),
                if Durable ->
                    ok = mnesia:write(web_durable_exchange,
                                      Exchange, write);
                   true -> ok
                end,
                Exchange;
          [ExistingX] -> ExistingX
        end
    end).

check_type(<<"fanout">>) ->
  fanout;
check_type(<<"direct">>) ->
  direct;
check_type(<<"topic">>) ->
  topic;
check_type(T) ->
  ?ERROR_MSG("invalid exchange type '~s'", [T]).

assert_type(#exchange{ type = ActualType }, RequiredType)
  when ActualType == RequiredType ->
  ok;
assert_type(#exchange{ name = Name, type = ActualType }, RequiredType) ->
  ?ERROR_MSG("cannot redeclare ~s of type '~s' with type '~s'", [Name, ActualType, RequiredType]).

lookup(Name) ->
  dirty_read({web_exchange, Name}).

lookup_or_die(Name) ->
  case lookup(Name) of
    {ok, X} -> X;
    {error, not_found} ->
      ?CRITICAL_MSG("no ~s", [Name]),
      exit({not_found, Name})
  end.

infos(Items, X) -> [{Item, i(Item, X)} || Item <- Items].

i(name,        #exchange{name        = Name})       -> Name;
i(type,        #exchange{type        = Type})       -> Type;
i(durable,     #exchange{durable     = Durable})    -> Durable;
i(auto_delete, #exchange{auto_delete = AutoDelete}) -> AutoDelete;
i(Item, _) -> throw({bad_argument, Item}).

info(X = #exchange{}) -> infos(?INFO_KEYS, X).

info(X = #exchange{}, Items) -> infos(Items, X).

route(X = #exchange{type = topic}, RoutingKey, Args) ->
  match_bindings(X, fun (#binding{key = BindingKey}) ->
                        topic_matches(BindingKey, RoutingKey)
                    end, RoutingKey, Args);

route(X = #exchange{type = fanout}, _RoutingKey, Args) ->
  match_routing_key(X, '_', Args);

route(X = #exchange{type = direct}, RoutingKey, Args) ->
  match_routing_key(X, RoutingKey, Args).


%% TODO: Maybe this should be handled by a cursor instead.
%% TODO: This causes a full scan for each entry with the same exchange
match_bindings(#exchange{name = Name}, Match, RoutingKey, Args) ->
  Query = qlc:q([QCallback || #route{binding = Binding = #binding{
                                                 exchange_name = ExchangeName,
                                                 callback = QCallback}} <-
                                mnesia:table(web_route),
                              ExchangeName == Name,
                              Match(Binding)]),
  Result = mnesia:async_dirty(fun qlc:e/1, [Query]),
  run_callbacks(Result, Args, RoutingKey).

match_routing_key(#exchange{name = Name}, RoutingKey, Args) ->
  MatchHead = #route{binding = #binding{exchange_name = Name,
                                        callback = '$1',
                                        key = RoutingKey,
                                        _ = '_'}},
  run_callbacks(mnesia:dirty_select(web_route, [{MatchHead, [], ['$1']}]), Args, RoutingKey).

run_callbacks(Callbacks, Args, RoutingKey) ->
  lists:foldl(
    fun(Callback, Acc) ->
        AccItem = case catch apply(Callback, Args) of
                    {'EXIT', Reason} ->
                      ?ERROR_MSG("~p~nrunning route: ~p", [Reason, RoutingKey]),
                      [];
                    stop ->
                      Acc;
                    ok ->
                      Acc;
                    Result ->
                      Result
                  end,
        [AccItem|Acc]
    end, [], Callbacks).

delete_bindings_for_exchange(ExchangeName) ->
  [begin
     ok = mnesia:delete_object(web_reverse_route,
                               reverse_route(Route), write),
     ok = delete_forward_routes(Route)
   end || Route <- mnesia:match_object(
                     web_route,
                     #route{binding = #binding{exchange_name = ExchangeName,
                                               _ = '_'}},
                     write)],
  ok.

delete_forward_routes(Route) ->
  ok = mnesia:delete_object(web_route, Route, write),
  ok = mnesia:delete_object(web_durable_route, Route, write).

has_bindings(ExchangeName) ->
  MatchHead = #route{binding = #binding{exchange_name = ExchangeName,
                                        _ = '_'}},
  try
    continue(mnesia:select(web_route, [{MatchHead, [], ['$_']}],
                           1, read))
  catch exit:{aborted, {badarg, _}} ->
      %% work around OTP-7025, which was fixed in R12B-1, by
      %% falling back on a less efficient method
      case mnesia:match_object(web_route, MatchHead, read) of
        []    -> false;
        [_|_] -> true
      end
  end.

continue('$end_of_table')    -> false;
continue({[_|_], _})         -> true;
continue({[], Continuation}) -> continue(mnesia:select(Continuation)).

call_with_exchange(Exchange, Fun) ->
  execute_mnesia_transaction(
    fun() -> case mnesia:read({web_exchange, Exchange}) of
               []  -> {error, exchange_not_found};
               [X] -> Fun(X)
             end
    end).

add_binding(ExchangeName, Callback, RoutingKey) ->
  call_with_exchange(
    ExchangeName,
    fun (X) ->
        ok = sync_binding(
               ExchangeName, Callback, RoutingKey,
               X#exchange.durable, fun mnesia:write/3)
    end).

delete_binding(ExchangeName, Callback, RoutingKey) ->
  call_with_exchange(
    ExchangeName,
    fun (X) ->
        ok = sync_binding(
               ExchangeName, Callback, RoutingKey,
               X#exchange.durable, fun mnesia:delete_object/3),
        maybe_auto_delete(X)
    end).

sync_binding(ExchangeName, Callback, RoutingKey, Durable, Fun) ->
  Binding = #binding{exchange_name = ExchangeName,
                     callback = Callback,
                     key = RoutingKey},
  ok = case Durable of
         true  -> Fun(web_durable_route,
                      #route{binding = Binding}, write);
         false -> ok
       end,
  {Route, ReverseRoute} = route_with_reverse(Binding),
  ok = Fun(web_route, Route, write),
  ok = Fun(web_reverse_route, ReverseRoute, write),
  ok.

route_with_reverse(#route{binding = Binding}) ->
  route_with_reverse(Binding);
route_with_reverse(Binding = #binding{}) ->
  Route = #route{binding = Binding},
  {Route, reverse_route(Route)}.

reverse_route(#route{binding = Binding}) ->
  #reverse_route{reverse_binding = reverse_binding(Binding)};

reverse_route(#reverse_route{reverse_binding = Binding}) ->
  #route{binding = reverse_binding(Binding)}.

reverse_binding(#reverse_binding{exchange_name = Exchange,
                                 callback = Callback,
                                 key = Key}) ->
  #binding{exchange_name = Exchange,
           callback = Callback,
           key = Key};

reverse_binding(#binding{exchange_name = Exchange,
                         callback = Callback,
                         key = Key}) ->
  #reverse_binding{exchange_name = Exchange,
                   callback = Callback,
                   key = Key}.

%% Pattern matching for routes
split_topic_key(Key) ->
  {ok, KeySplit} = regexp:split(binary_to_list(Key), "\\."),
  KeySplit.

topic_matches(PatternKey, RoutingKey) ->
  P = split_topic_key(PatternKey),
  R = split_topic_key(RoutingKey),
  topic_matches1(P, R).

topic_matches1(["#"], _R) ->
  true;
topic_matches1(["#" | PTail], R) ->
  last_topic_match(PTail, [], lists:reverse(R));
topic_matches1([], []) ->
  true;
topic_matches1(["*" | PatRest], [_ | ValRest]) ->
  topic_matches1(PatRest, ValRest);
topic_matches1([PatElement | PatRest], [ValElement | ValRest]) when PatElement == ValElement ->
  topic_matches1(PatRest, ValRest);
topic_matches1(_, _) ->
  false.

last_topic_match(P, R, []) ->
  topic_matches1(P, R);
last_topic_match(P, R, [BacktrackNext | BacktrackList]) ->
  topic_matches1(P, R) or last_topic_match(P, [BacktrackNext | R], BacktrackList).

delete(ExchangeName, _IfUnused = true) ->
  call_with_exchange(ExchangeName, fun conditional_delete/1);
delete(ExchangeName, _IfUnused = false) ->
  call_with_exchange(ExchangeName, fun unconditional_delete/1).

maybe_auto_delete(#exchange{auto_delete = false}) ->
  ok;
maybe_auto_delete(Exchange = #exchange{auto_delete = true}) ->
  conditional_delete(Exchange),
  ok.

conditional_delete(Exchange = #exchange{name = ExchangeName}) ->
  case has_bindings(ExchangeName) of
    false  -> unconditional_delete(Exchange);
    true   -> {error, in_use}
  end.

unconditional_delete(#exchange{name = ExchangeName}) ->
  ok = delete_bindings_for_exchange(ExchangeName),
  ok = mnesia:delete({web_durable_exchange, ExchangeName}),
  ok = mnesia:delete({web_exchange, ExchangeName}).

%%----------------------------------------------------------------------------
%% EXTENDED API
%% These are API calls that are not used by the server internally,
%% they are exported for embedded clients to use

%% This is currently used in mod_rabbit.erl (XMPP) and expects this to
%% return {QueueName, RoutingKey, Arguments} tuples
list_exchange_bindings(ExchangeName) ->
  Route = #route{binding = #binding{exchange_name = ExchangeName,
                                    _ = '_'}},
  [{Callback, RoutingKey} ||
    #route{binding = #binding{callback = Callback,
                              key = RoutingKey}}
      <- mnesia:dirty_match_object(web_route, Route)].

                                                % Refactoring is left as an exercise for the reader


%% Extended mnesia helpers
dirty_read(ReadSpec) ->
    case mnesia:dirty_read(ReadSpec) of
        [Result] -> {ok, Result};
        []       -> {error, not_found}
    end.

dirty_read_all(TableName) ->
  mnesia:dirty_select(TableName, [{'$1',[],['$1']}]).

dirty_foreach_key(F, TableName) ->
  dirty_foreach_key1(F, TableName, mnesia:dirty_first(TableName)).

dirty_foreach_key1(_F, _TableName, '$end_of_table') ->
  ok;
dirty_foreach_key1(F, TableName, K) ->
  case catch mnesia:dirty_next(TableName, K) of
    {'EXIT', _} ->
      aborted;
    NextKey ->
      F(K),
      dirty_foreach_key1(F, TableName, NextKey)
  end.

execute_mnesia_transaction(TxFun) ->
  %% Making this a sync_transaction allows us to use dirty_read
  %% elsewhere and get a consistent result even when that read
  %% executes on a different node.
  case mnesia:sync_transaction(TxFun) of
    {atomic,  Result} -> Result;
    {aborted, Reason} -> throw({error, Reason})
  end.


%%
%% Tests
%%
