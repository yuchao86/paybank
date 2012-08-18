%%%-------------------------------------------------------------------
%%% File    : eb_server.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Editr   : Charles Feduke
%%% Description : The ErlyBank account server.
%%%
%%% Created :  5 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Modified: 16 Jun 2012
%%%-------------------------------------------------------------------
-module(eb_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
		create_account/2,
		destroy_account/1,
		deposit/2,
		withdraw/2,
		authorize/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Args) ->
	{ok, dict:new()}. 

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({authorize, Name, Pin}, _From, State) ->
	case dict:find(Name, State) of
		{ok, {{pin, Pin}, _}} ->
			{reply, ok, State};
		{ok, _} ->
			{reply, {error, invalid_pin}, State};
		error ->
			{reply, {error, account_does_not_exist}, State}
	end;
handle_call({deposit, Name, Amount}, _From, State) ->
	case dict:find(Name, State) of
		{ok, {Pin, {balance, Value}}} ->
			NewBalance = Value + Amount,
			Response = {ok, NewBalance},
			NewState = dict:store(Name, {Pin, {balance, NewBalance}}, State),
			{reply, Response, NewState};
		error ->
			{reply, {error, account_does_not_exist}, State}
	end;
handle_call({withdraw, Name, Amount}, _From, State) ->
	case dict:find(Name, State) of
		{ok, {_, {balance, Value}}} when Value < Amount ->
			{reply, {error, not_enough_funds}, State};
		{ok, {Pin, {balance, Value}}} ->
			NewBalance = Value - Amount,
			Response = {ok, NewBalance},
			NewState = dict:store(Name, {Pin, {balance, NewBalance}}, State),
			{reply, Response, NewState};
		error ->
			{reply, {error, account_does_not_exist}, State}
	end;
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({create, Name, Pin}, State) ->
	{noreply, dict:store(Name, {{pin, Pin}, {balance, 0}}, State)};
handle_cast({destroy, Name}, State) ->
	{noreply, dict:erase(Name, State)};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

create_account(Name, Pin) ->
	gen_server:cast(?SERVER, {create, Name, Pin}).

authorize(Name, Pin) ->
	gen_server:call(?SERVER, {authorize, Name, Pin}).

deposit(Name, Amount) ->
	gen_server:call(?SERVER, {deposit, Name, Amount}).

withdraw(Name, Amount) ->
	gen_server:call(?SERVER, {withdraw, Name, Amount}).

destroy_account(Name) ->
	gen_server:cast(?SERVER, {destroy, Name}).
