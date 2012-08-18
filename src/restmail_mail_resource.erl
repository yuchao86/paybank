-module(restmail_mail_resource).
-export([init/1,
	 allowed_methods/2,
	 process_post/2
	 ]).
	 
-include("restmail.hrl").

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {{trace, "/tmp"}, undefined}.

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

process_post(Req, State) ->
    {struct, PropList} = mochijson2:decode(wrq:req_body(Req)),
    send(proplists:get_value(<<"recipients">>, PropList),
	 proplists:get_value(<<"subject">>, PropList),
	 proplists:get_value(<<"body">>, PropList)),
    {true, Req, State}.

send([Addr | Tail], Subject, Body) ->
    spawn(fun() ->
		  gen_smtp_client:send({?SMTP_FROM_ADDR,
					[Addr],
					mimemail:encode({<<"text">>,
							 <<"html">>,
							 [{<<"From">>, ?SMTP_FROM_HEADER},
							  {<<"To">>, Addr},
							  {<<"Subject">>, Subject}],
							 [],
							 Body})
				       },
				       ?SMTP_OPTIONS)
	  end),
    send(Tail, Subject, Body);
send([], _, _) ->
    ok.
