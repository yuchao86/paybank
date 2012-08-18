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

-type name() :: atom().
-type write_function() :: fun((iodata()) -> 'ok' | {'error', term()}).
-opaque writer() :: pid() | atom().

%% TODO: it seems there is no way to state that element_name must be non-empty!
-type namespace_prefix()    :: binary() | string().
-type namespace_uri()       :: binary() | string().
-type element_name()        :: iodata().
-type ns()                  :: iodata().

-record(stack_frame, {
    ns                      :: ns(),
    local_name              :: element_name(),
    has_attributes          :: boolean(),
    has_children    = false :: boolean()
}).

-record(writer, {
    ns          = []        :: list(tuple(namespace_prefix(), namespace_uri())),
    ns_stack    = []        :: list(namespace_uri()),
    stack       = []        :: [#stack_frame{}],
    quote       = <<"\"">>  :: binary(),
    prettyprint = false     :: boolean(),
    indent      = <<"\t">>  :: binary(),
    newline     = <<"\n">>  :: binary(),
    encoding                :: atom(),
    last_error              :: term(),
    %escapes = [
    %    {<<"\"">>,
    %     {binary:compile_pattern(<<"\"">>),
    %     <<"\\\"">>}}
    %]
    write                   :: write_function(),
    format,
    close
}).
