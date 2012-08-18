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
-module(xml_writer_fsm_props).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-include("xml_writer.hrl").

-compile(export_all).

%%
%% fsm transitions
%%

waiting_for_input(_W) ->
    %% TODO: loosen up the type constraints here....
     [{waiting_for_input,
         {call, xml_writer, add_namespace,
             [?MODULE, 
                 test_helper:namespace_prefix(),
                 test_helper:url()]}},
      {history,
        {call, xml_writer, write_value, 
            [?MODULE, test_helper:valid_latin_string()]}},
      {element_started,
        {call, xml_writer, start_element, 
            [?MODULE, test_helper:valid_latin_string()]}}].

element_started(_W) ->
    [{waiting_for_input, {call, xml_writer, end_element, [?MODULE]}},
     {element_started, {call, xml_writer, write_value, 
        [?MODULE, test_helper:valid_latin_string()]}}].

initial_state() -> waiting_for_input.

initial_state_data() -> #writer{}.

next_state_data(_, _, S,
                _Result, {call, xml_writer, add_namespace, [_, NS, NSUri]}) ->
    {_, _, _, S2} = xml_writer:waiting_for_input({add_namespace, 
                                                    NS, NSUri}, undefined, S),
    S2;
next_state_data(waiting_for_input, element_started, S,
                Result, {call, xml_writer, start_element, [_, Elem]}) ->
    case Result of
        ok ->
            S#writer{ stack=[#stack_frame{ local_name=Elem }|S#writer.stack] };
        _ ->
            S
    end;
next_state_data(_From, _Target, S, Result, {call, _, end_element, _}) ->
    case Result of
        ok ->
            S#writer{ stack=erlang:tl(S#writer.stack)};
        _ ->
            S
    end;
next_state_data(_From, _Target, S, _Result, {call, _, _, _}) ->
    S.

%precondition(waiting_for_input, _, W, {call, _, write_value, _}) ->
%    true; % length(W#writer.stack) > 0.
%precondition(waiting_for_input, stopping, W, {_, _, _, _}) ->
%    false;
%precondition(stopping, stopping, _, _) ->
%    true.

%precondition(Today, _, S, {call,_,hungry,[]}) ->
%    case Today of
%    cheese_day ->
%        S#storage.cheese > 0;
%    lettuce_day ->
%        S#storage.lettuce > 0;
%    grapes_day ->
%        S#storage.grapes > 0
%    end;

%precondition(waiting_for_input, element_started,
%                S, {call, _, start_element, _}) ->
%    length(S#writer.stack) > 0;
precondition(FromState, ToState, S, {call, _, F, _}) ->
    case FromState of
        waiting_for_input ->
            case lists:member(F, [write_value, end_element]) of
                true ->
                    length(S#writer.stack) > 0;
                false ->
                    %% TODO: tighten this up...
                    true
            end;
        _ ->
            true
    end.

postcondition(waiting_for_input, _, _,
                {call, _, add_namespace, _}, Result) ->
%    FoundNS = lists:keyfind(NS, 1, W#writer.ns),
%    error_logger:info_msg("FoundNS: ~p in ~p~n", [FoundNS, W]),
%    FoundNS == {NS, NSUri} andalso
    Result =:= ok;
postcondition(waiting_for_input, _, #writer{stack=[]},
                {call, _, write_value, _}, Result) ->
    Result == {error, no_root_node};
postcondition(element_started, waiting_for_input,
                W, {call, _, _, _}, _Result) ->
    length(W#writer.stack) > 0;
postcondition(_, _, _, _, Result) ->
    Result =:= ok.

% weight(waiting_for_input, _NewState, {call, _, _, _}) -> 5;
% weight(_, _, _) -> 1.

prop_all_state_transitions_are_valid() ->
    ?FORALL(Cmds, proper_fsm:commands(?MODULE),
        begin
        Path = filename:join([rebar_utils:get_cwd(), ".test",
                                atom_to_list(?MODULE)]),
        filelib:ensure_dir(Path),
        Writer = xml_writer:new(?MODULE, fun(_) -> ok end),
        {History, State, Result} =
            proper_fsm:run_commands(?MODULE, Cmds),
        xml_writer:close(Writer),
        ?WHENFAIL(
           %io:format("History: ~w\nState: ~w\nResult: ~w\n",
           %         [History, State, Result]),
           proper_report:report(Cmds, History, State, Result),
           aggregate(zip(proper_fsm:state_names(History),
                 command_names(Cmds)),
                 Result =:= ok))
        end).
