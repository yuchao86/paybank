%% @doc Callbacks for the restmail application.

-module(restmail_app).
-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for restmail.
start(_Type, _StartArgs) ->
    restmail_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for restmail.
stop(_State) ->
    ok.
