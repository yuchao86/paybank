%% -----------------------------------------------------------------------------
%%
%% xml_writer: test suites
%%
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
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
-module(xml_writer_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("xmerl/include/xmerl.hrl").

namespace_handling_test_xx() ->
    {"Namespace handling",
         [{"tags should be put into their proper namespace",
            fun() ->
                FileId = test_helper:tmp_file_id(),
                NS = "ns1",
                NSUri = "http://foo.bar.baz/schemas/2011",
                Writer = xml_writer:file_writer(FileId),
                WithNS = xml_writer:add_namespace(NS, NSUri, Writer),
                FinalNode = xml_writer:write_node(NS, "foobar", WithNS),
                xml_writer:close(FinalNode),
                {ok, Bin} = file:read_file(FileId),
                XMLNode = binary_to_list(Bin),
                ?assertThat(XMLNode, is_in_namespace(NS, NSUri, "foobar"))
                end
            }]}.

%%
%% Custom Hamcrest Matchers
%%

is_in_namespace(NS, NSUri, NodeName) ->
    UriAsAtom = list_to_atom(NSUri),
    fun(Actual) ->
        case catch(xmerl_scan:string(Actual, [{encoding, latin1}])) of
            {#xmlElement{
                expanded_name=ExName,
                nsinfo={NS, NodeName},
                namespace=#xmlNamespace{nodes = [{NS,UriAsAtom}]}
            },_} ->
                ExName == list_to_atom(NS ++ ":" ++ NodeName);
            Other ->
                rebar_log:log(warn, "Parsing ~p failed: ~p~n", [Actual, Other]),
                false
        end
    end.
