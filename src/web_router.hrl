-record(exchange, {name, type, durable, auto_delete}).

%% mnesia doesn't like unary records, so we add a dummy 'value' field
-record(route, {binding, value = const}).
-record(reverse_route, {reverse_binding, value = const}).
-record(binding, {exchange_name, key, callback}).
-record(reverse_binding, {callback, key, exchange_name}).

-type(exchange_name() :: atom()).
-type(exchange_type() :: 'direct' | 'topic' | 'fanout').
-type(routing_key() :: binary()).
-type(binding_key() :: binary()).

-type(exchange() ::
      #exchange{name        :: exchange_name(),
                type        :: exchange_type(),
                durable     :: bool(),
                auto_delete :: bool()}).
-type(binding() ::
      #binding{exchange_name    :: exchange_name(),
               key              :: binding_key(),
               callback :: function()}).
-type(not_found() :: {'error', 'not_found'}).
