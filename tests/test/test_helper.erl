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
-module(test_helper).
-compile(export_all).

-include_lib("proper/include/proper.hrl").

%%
%% PropEr Types
%%

valid_latin_string() ->
    non_empty(list(proper_stdgen:latin_char())).

url() ->
    ?LET({Host, Label}, {proper_stdgen:hostname(), proper_stdgen:label()},
        "http://" ++ Host ++ "/" ++ Label).

namespace_prefix() ->
    ?SIZED(3, non_empty(list(proper_stdgen:latin_char()))).

%%
%% Utilities
%%

tmp_file_id() ->
    Val = case get(current) of
        undefined ->
            put(current, random:uniform(1000000)),
            get(current);
        Other ->
            NextVal = Other + 1,
            put(current, NextVal), NextVal
    end,
    integer_to_list(Val).

output(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_list(Bin).
