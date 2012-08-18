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
-module(xml_writer_props).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-compile(export_all).

%%
%% Properties
%%

x_prop_basic_iolist_serialisation() ->
    ?FORALL(IoData, iodata(),
          ?IMPLIES(length(IoData) > 0,
              enforce(fun has_iodata_content/2,
                    test_helper:tmp_file_id(), IoData))).

x_prop_basic_binary_serialisation() ->
    ?FORALL(Value, a_to_z(),
        ?IMPLIES(length(Value) > 1,
            enforce(fun inner_text_value/2,
                    test_helper:tmp_file_id(), Value))).

x_prop_basic_atom_serialisation() ->
    ?FORALL(Value, a_to_z(),
        ?IMPLIES(length(Value) > 1,
            enforce(fun has_named_value_foo/2, "foo",
                    test_helper:tmp_file_id(), Value))).

x_prop_format_floating_point_numbers() ->
    ?FORALL(F, float(),
        begin
            File = filename:join(".test", test_helper:tmp_file_id()),
            io:format("Writing to ~s~n", [filename:join(rebar_utils:get_cwd(), File)]),
            Writer = xml_writer:file_writer(File, [write]),
            Writer2 = xml_writer:with_element("price", Writer,
                fun(W) ->
                    W = xml_writer:format("~f", [F], W),
                    io:format("W = ~p~n", [W]),
                    W
                end
            ),
            io:format("Writer2 = ~p~n", [Writer2]),
            xml_writer:close(Writer2),
            inner_text_value(File, F)
        end
    ).

%%
%% Custom Hamcrest Matchers
%%

has_inner_text_value(V) ->
    fun(Actual) ->
        case catch(xmerl_scan:string(Actual, [{encoding, latin1}])) of
            {#xmlElement{content=[#xmlText{value=V}|_]},_} -> true;
            Other ->
                ct:pal("Parsing ~p failed: ~p~n", [Actual, Other]),
                false
        end
    end.

has_content_inside(_Node) ->
    fun(Actual) ->
        case catch(xmerl_scan:string(Actual)) of
            {#xmlElement{
                content=[#xmlText{value=Value}|_]},_} ->
                length(Value) > 0;
            Other ->
                ct:pal("Parsing ~p failed: ~p~n", [Actual, Other]),
                false
        end
    end.

match_named_node_value(Node, Value) ->
    fun(Actual) ->
        case catch(xmerl_scan:string(Actual)) of
            {#xmlElement{name=Node,
                content=[#xmlText{value=Value}|_]},_} ->
                true;
            Other ->
                ct:pal("Parsing ~p failed: ~p~n", [Actual, Other]),
                false
        end
    end.

%%
%% Abstract Test Functions
%%

has_iodata_content(FileId, _) ->
    assert_that(test_helper:output(filename:join(".test", FileId)),
                has_content_inside(iodata)).

has_named_value_foo(FileId, Value) ->
    assert_that(test_helper:output(filename:join(".test", FileId)),
                match_named_node_value(foo, Value)).

inner_text_value(FileId, Value) ->
    assert_that(test_helper:output(filename:join(".test", FileId)),
                has_inner_text_value(Value)).

enforce(Enforcement, FileId, Value) ->
    enforce(Enforcement, "data", FileId, Value).

enforce(Enforcement, Elem, FileId, Value) ->
    if length(Value) > 0 ->
        Writer = xml_writer:file_writer(filename:join(".test", FileId)),
        xml_writer:with_element(Elem, Writer, fun(W) ->
           xml_writer:write_value(Value, W)
        end),
        Enforcement(FileId, Value)
    end.

%%
%% PropEr Type Definitions
%%

a_to_z() ->
    %% NB: make sure xmerl (which we're using in the matchers)
    %% doesn't fall on it's backside complaining about encodings and so on
    %% TODO: this is far too conservative, so we'll need to broaden it later
    non_empty(list(integer(97, 122))).

node_data() ->
    union([a_to_z(), noshrink(non_empty(utf8_binary()))]).

iodata() ->
    %% TODO: find out whether any new(er) version of PropEr supports iolist()
    %% TODO: relax the matchers so we can remove the noshrink constraints
    union([noshrink(non_empty(list(noshrink(non_empty(list(node_data())))))),
        noshrink(non_empty(list(node_data())))]).

utf8_binary() ->
  ?LET(L, list(a_to_z()),
    unicode:characters_to_binary(L, utf8)).
