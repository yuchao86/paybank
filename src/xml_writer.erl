%% -----------------------------------------------------------------------------
%% Copyright (c) 2002-2011 Tim Watson (watson.timothy@gmail.com)
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
%% -----------------------------------------------------------------------------
-module(xml_writer).
-behaviour(gen_fsm).

%% API exports
-export([file_writer/1, file_writer/2, file_writer/3]).
-export([new/1, new/2, new/3, new/4]).
-export([close/1]).
-export([add_namespace/3]).
-export([write_value/2, start_element/2, end_element/1]).

%% gen_fsm state-name exports
-export([waiting_for_input/3, element_started/3]).

%% gen_fsm behaviour exports
-export([init/1,
         handle_sync_event/4,
         terminate/3,
         handle_event/3,
         handle_info/3,
         code_change/4]).

-include("xml_writer.hrl").

-define(OPEN_BRACE, <<"<">>).
-define(FWD_SLASH, <<"/">>).
-define(CLOSE_BRACE, <<">">>).
-define(SPACE, <<" ">>).

-define(OPEN_ELEM(N), [?OPEN_BRACE, N, ?CLOSE_BRACE]).
-define(CLOSE_ELEM, [?FWD_SLASH, ?CLOSE_BRACE]).
-define(CLOSING_ELEM(N), [?OPEN_BRACE, ?FWD_SLASH, N, ?CLOSE_BRACE]).

%%
%% API
%%

file_writer(Path) ->
    file_writer(Path, [binary, append]).

file_writer(Path, Modes) ->
    {ok, IoDevice} = file:open(Path, Modes),
    new(fun(X) -> file:write(IoDevice, X) end,
        fun(M, A, W) -> io:format(IoDevice, M, A), W end,
        fun(_) -> file:close(IoDevice) end).

file_writer(Name, Path, Modes) ->
    {ok, IoDevice} = file:open(Path, Modes),
    new(Name,
        fun(X) -> file:write(IoDevice, X) end,
        fun(M, A, W) -> io:format(IoDevice, M, A), W end,
        fun(_) -> file:close(IoDevice) end).

new(Name, WriteFun, FormatFun, CloseFun) ->
    {ok, _Pid} = gen_fsm:start({local, Name}, ?MODULE,
                                [WriteFun, FormatFun, CloseFun], []),
    Name.

new(WriteFun, FormatFun, CloseFun) ->
    {ok, Pid} = gen_fsm:start(?MODULE, [WriteFun, FormatFun, CloseFun], []),
    Pid.

-spec new(name(), write_function()) -> writer().
new(Name, WriteFun) ->
    {ok, _Pid} = gen_fsm:start({local, Name}, ?MODULE, [WriteFun], []),
    Name.

-spec new(write_function()) -> writer().
new(WriteFun) ->
    {ok, Pid} = gen_fsm:start(?MODULE, [WriteFun], []),
    Pid.

-spec close(writer()) -> term().
close(Writer) ->
    gen_fsm:sync_send_all_state_event(Writer, stop).

%% namespace handling

-spec add_namespace(writer(), namespace_prefix(), namespace_uri()) -> ok.
add_namespace(Writer, NS, NSUri) ->
    gen_fsm:sync_send_event(Writer, {add_namespace, NS, NSUri}).

-spec write_value(writer(), iodata()) -> 'ok' | {'error', term()}.
write_value(Writer, Value) ->
    gen_fsm:sync_send_event(Writer, {write_value, Value}).

-spec start_element(writer(), element_name()) -> ok.
start_element(Writer, ElementName) ->
    gen_fsm:sync_send_event(Writer, {start_element, ElementName}).

-spec end_element(writer()) -> 'ok'.
end_element(Writer) ->
    gen_fsm:sync_send_event(Writer, end_element).

%%
%% gen_fsm callbacks
%%

init([Writer]) ->
    {ok, waiting_for_input, #writer{ write=Writer }};
init([Writer, FormatFun, CloseFun]) ->
    {ok, waiting_for_input,
        #writer{ write = Writer,
                 format = FormatFun,
                 close = CloseFun}}.

waiting_for_input({write_value, _}, _From, W=#writer{ stack=[] }) ->
    {reply, {error, no_root_node}, waiting_for_input, W};
waiting_for_input({write_value, Val}, _From, W) ->
    {reply, ok, waiting_for_input, write(Val, W)};
waiting_for_input(end_element, From, W) ->
    element_started(end_element, From, W);
waiting_for_input({start_element, ElementName}, From, W) ->
    NewState = push(ElementName, W),
    gen_fsm:reply(From, ok),
    {next_state, element_started, NewState};
waiting_for_input({add_namespace, NS, NSUri}, _From,
                    W=#writer{ ns_stack=NSStack, ns=NSMap }) ->
    NewState = W#writer{
        ns=lists:keystore(NS, 1, NSMap, {NS, NSUri}),
        ns_stack=[NS|NSStack]
    },
    {reply, ok, waiting_for_input, NewState}.

element_started({write_value, Val}, _From, W) ->
    {reply, ok, waiting_for_input, write([?CLOSE_BRACE, Val], W)};
element_started(end_element, _From, Writer) ->
    {reply, ok, waiting_for_input, pop(Writer)}.

handle_sync_event(stop, _From, _StateName, _StateData) ->
    {stop, normal, ok, []}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Info, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%
%% Internal API
%%

push(FR=#stack_frame{ns=ElemNS, local_name=ElemName},
        W=#writer{ stack=S, ns=NSMap, ns_stack=NSStack }) ->
    %% TODO: deal with nested nodes
    write(start_elem(qname(ElemNS, ElemName), W), W),
    %% TODO: move namespace handling out into another state
    %% TODO: track namespace declarations and do not duplicate them.
    case NSStack of
        [] -> W;
        [_|_] ->
            XmlnsAtt =
                [ setelement(1, lists:keyfind(NSEntry, 1, NSMap),
                    "xmlns:" ++ NSEntry) || NSEntry <- NSStack ],
            write_attributes(XmlnsAtt, W)
    end,
    W#writer{ stack=[FR|S] };
push(ElementName, W) ->
    push(#stack_frame{ local_name=ElementName }, W).

qname(undefined, Name) ->
    Name;
qname(NS, Name) ->
    [NS, <<":">>, Name].

pop(Writer=#writer{ stack=[] }) ->
    Writer;
pop(W=#writer{ stack=[SF=#stack_frame{ns=NS, local_name=Name}|Stack] }) ->
    case SF#stack_frame.has_children of
        true ->
            write(?CLOSING_ELEM(qname(NS, Name)), W);
        false ->
            write(?CLOSE_ELEM, W)
    end,
    W#writer{ stack=Stack }.

write_attributes(AttributeList, Writer) ->
    lists:foldl(fun({Name, Value}, XMLWriter) ->
        write_attribute(Name, Value, XMLWriter)
    end, Writer, AttributeList).

write_attribute(Name, Value, Writer) ->
    write_attribute(undefined, Name, Value, Writer).

write_attribute(NS, Name, Value, Writer=#writer{ quote=Quot }) ->
    write([?SPACE, qname(NS, Name), <<"=">>, Quot, Value, Quot], Writer).

write(Data, Writer=#writer{ write=WriteFun }) when is_function(WriteFun, 1) ->
    case WriteFun(encode(Data, Writer)) of
        ok ->
            Writer;
        Error ->
            exit(Error)
    end.

encode(Data, #writer{ encoding=undefined }) when is_atom(Data) ->
    atom_to_binary(Data, utf8);
encode(Data, #writer{ encoding=Encoding }) when is_atom(Data) ->
    atom_to_binary(Data, Encoding);
encode(Data, #writer{ encoding=_ }) ->  %% when is_binary(Data) ->
    Data.  %% TODO: deal with in/out encoding requirements
    %% HINT: maybe pass this in from things like Content Type/Disposition headers

start_elem(NodeName, #writer{ prettyprint=false }) ->
    [?OPEN_BRACE, NodeName];
start_elem(NodeName, #writer{ stack=S, prettyprint=true, indent=I, newline=NL }) ->
    %% TODO: consider a format string for this...
    [NL, lists:duplicate(length(S), I), ?OPEN_BRACE, NodeName].
