%%%-------------------------------------------------------------------
%%% File    : web_router_sup.erl
%%% Author  : John Long <machinist@asceth.com>
%%% Description : Simply creates mnesia tables if not already existing
%%%                 and recovers any durable exchanges/routes.
%%%
%%% Created :  5 Feb 2009 by John Long <machinist@asceth.com>
%%%-------------------------------------------------------------------
-module(web_router_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
  web_router_mnesia:init(),
  web_exchange:recover(),
  {ok, {{one_for_one, 10, 10}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================

