%% ---------------------------------
%% Logging mechanism

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

-define(DEBUG(Format, Args),
    gen_event:notify(error_logger, {info_msg, group_leader(), {self(), "D(~p:~p:~p) : " ++ Format ++ "~n", [self(), ?MODULE, ?LINE] ++ Args}})).

-define(INFO_MSG(Format, Args),
    gen_event:notify(error_logger, {info_msg, group_leader(), {self(), "I(~p:~p:~p) : " ++ Format ++ "~n", [self(), ?MODULE, ?LINE] ++ Args}})).

-define(WARNING_MSG(Format, Args),
    gen_event:notify(error_logger, {error, group_leader(), {self(), "W(~p:~p:~p) : " ++ Format ++ "~n", [self(), ?MODULE, ?LINE] ++ Args}})).

-define(ERROR_MSG(Format, Args),
    gen_event:notify(error_logger, {error, group_leader(), {self(), "E(~p:~p:~p) : " ++ Format ++ "~n", [self(), ?MODULE, ?LINE] ++ Args}})).

-define(CRITICAL_MSG(Format, Args),
    gen_event:notify(error_logger, {error, group_leader(), {self(), "C(~p:~p:~p) : " ++ Format ++ "~n", [self(), ?MODULE, ?LINE] ++ Args}})).
