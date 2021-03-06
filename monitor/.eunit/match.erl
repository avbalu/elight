%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc multi purpose log utility module
%%% Does prcoessing of filters, matching the conditions and doing the
%%% actions.
%% The filters are sent to session by monitor
%% All monitors have a copy of filters
%% When Filters are modified all monitors need to be updated
%% There are two kinds of fields in the filter
%% 1) client, server, proto and port, latencies - These can be 
%% matched by session  session update
%%% 2) average and peak latencies - 
%%%             These can be matched by admin during 
%%%           session_update. 
%%%        In this case there is no need to log the session traffic
%%%    as this session
%%%     is just the last straw and the root cause
%%% When logging needs to be done it is done in the context of session 
%%% in cases 1) and 2) and by admin in cases 3)
%%% Note that 1) & 2) are independant from 3) and their logging events 
%%% seperate
%%% This is not a process
%%% This code runs in the context of session processes
%%% POR = Port Or Range
%%  Possible optimization:  ipAddress in condition can be masked by L.
%%       This can be done during config parsing and strored in 
%%       configdb.
%%% @end
%%% Created : 21 Jan 2015 by adoor balasubramanian <balu@localhost.localdomain>

%%%-------------------------------------------------------------------
-module(match).

-export([match/2]).

-include("../../common/common.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../../api/gen-erl/api_types.hrl").


-spec match(SU :: #session_update{},
	    Filters :: [#filter{}]) ->
		   Filter :: #filter{}   | none.
match(SU, Filters) ->
    L = lists:dropwhile(fun(#filter{condition = C})->
				not match_one_filter(SU, C)
			end, Filters),

    match(L).

match([]) ->
    none;
match([Filter|_]) ->
    Filter.


match_latency(undefined, _L) ->
    true;
match_latency(_Threshold, undefined) ->
    false;
match_latency(Threshold, Value) ->
    Value >= Threshold.




match_tr_latency(undefined, _Trs) ->
    true;
match_tr_latency(TrL_Threshold, Trs) ->
    F = fun(Latency) -> 
		match_latency(TrL_Threshold,Latency) 
	end,
    Latencies = [X || #transaction{latency = X} <- Trs],
    lists:any(F, Latencies).

	

-spec match_one_filter(SU :: #session_update{}, 
		       Condition :: #condition{}) ->
			      boolean().

match_one_filter(#session_update{
		    session = #session{client = C,
				       server = S,
				       port = P},
		    latency = CL, 
		    error_code = Err,
		    transactions = Trs
		   },
		 #condition{client = NC,
			    server = NS,
			    portOrRange = POR,
			    connectionLatency = CL_Threshold,
			    latency = TrL_Threshold,
			    error = CErr
			   }) ->
    match_ip(C,NC) andalso
	match_ip(S, NS) andalso
	match_por(POR, P) andalso
	match_latency(CL_Threshold, CL) andalso
	match_error_code(CErr, Err) andalso
	match_tr_latency(TrL_Threshold, Trs).
			    
match_error_code(undefined, _) ->
    true;
match_error_code(_, undefined) ->
    false;
match_error_code(CErr, Err) ->
    CErr =:= admin:api_error_code(Err).

match_por(undefined, _P) ->
    true;
match_por(#portOrRange{port = undefined, 
		       range = undefined}, _P) ->
    true;
match_por(#portOrRange{port = undefined, 
		       range = #range{startItem = S,
				      endItem = E}}, P) ->
    P >= S andalso P =< E;
match_por(#portOrRange{port = #compare{op = Op,
				       port = ConditionP},
		       range = undefined}, P) ->
    match_port(ConditionP,Op,P).

match_port(ConditionP, ?api_Op_LESS, SessionP) ->
    SessionP < ConditionP;
match_port(ConditionP, ?api_Op_MORE, SessionP) ->
    SessionP > ConditionP;
match_port(P, _Op, P) ->
    true;
match_port(_ConditionP, _Op, _SessionP) ->
    false.


match_ip(_IP, undefined) ->
    true;
match_ip(IP, #network{ipAddress = IP1, mask = L}) ->
    L1 = 32 - L,
    <<N:L,_:L1>> = IP,
    <<N1:L,_:L1>> = IP1,
    N =:= N1.

%% Tests
%% match one filter tests
mof1_test() ->
    Client = #network{ipAddress = <<10,10,10,2>>, mask = 32},
    false = match_one_filter(#session_update{session = #session{client= <<10,0,0,1>>}},
		     #condition{client = Client}).

mof2_test() ->
    Client = #network{ipAddress = <<10,10,10,2>>, mask = 32},
    true = match_one_filter(#session_update{session = #session{client = <<10,10,10,2>>}},
			     #condition{client = Client}).

mof3_test() ->
    Client = #network{ipAddress = <<10,10,10,2>>, mask = 24},
    true = match_one_filter(#session_update{session = #session{client= <<10,10,10,1>>}},
			    #condition{client = Client}).

port1_test() ->
    true = match_one_filter(#session_update{session = #session{ port = 80}},
		     #condition{portOrRange =  #portOrRange{}}).

port2_test() ->
    POR = #portOrRange{port = #compare{op = ?api_Op_LESS, 
					port = 80}},
    C = #condition{portOrRange =  POR},
   false = match_one_filter(#session_update{session = #session{ port = 80}}, C).

port3_test() ->
    POR = #portOrRange{port = #compare{op = ?api_Op_LESS, 
					port = 81}},
    C = #condition{portOrRange =  POR},
   true = match_one_filter(#session_update{session = #session{ port = 80}}, C).

port_server_client_test() ->
    Client = #network{ipAddress = <<10,10,10,0>>, mask = 30},
    Server = #network{ipAddress = <<74,125,239,134>>, mask = 32},
    POR = #portOrRange{port = #compare{op = ?api_Op_EQ, 
					port = 80}},
    C = #condition{portOrRange =  POR,
		  client = Client,
		  server =  Server},
    true = match_one_filter(#session_update{session = #session{ port = 80,
				      client = <<10,10,10,1>>,
				      server = <<74,125,239,134>>}}, C).

port_server_client_false_test() ->
    Client = #network{ipAddress = <<10,10,10,1>>, mask = 30},
    Server = #network{ipAddress = <<74,125,239,134>>, mask = 32},
    POR = #portOrRange{port = #compare{op = ?api_Op_LESS, 
					port = 80}},
    C = #condition{portOrRange =  POR,
		  client = Client,
		  server =  Server},
    false = match_one_filter(#session_update{session = #session{ port = 80,
				      client = <<10,0,0,1>>,
				      server = <<74,125,239,134>>}}, C).
 
port_range_test() ->
    Client = #network{ipAddress = <<10,10,10,1>>, mask = 30},
    Server = #network{ipAddress = <<74,125,239,134>>, mask = 32},
    POR = #portOrRange{range = #range{startItem = 79,
					  endItem = 81}},
    C = #condition{portOrRange =  POR,
		  client = Client,
		  server = Server },
    false = match_one_filter(#session_update{session = #session{ port = 82,
				      client = <<10,10,10,1>>,
				      server = <<74,125,239,134>>}}, C),
    false = match_one_filter(#session_update{session = #session{ port = 78,
				      client = <<10,10,10,1>>,
				      server = <<74,125,239,134>>}}, C),
    false = match_one_filter(#session_update{session = #session{ port = 78,
				      client = <<10,10,10,1>>,
				      server = <<74,125,239,134>>}}, C),
    true = match_one_filter(#session_update{session = #session{ port = 79,
				      client = <<10,10,10,1>>,
				      server = <<74,125,239,134>>}}, C),
    true = match_one_filter(#session_update{session = #session{ port = 81,
				      client = <<10,10,10,1>>,
				      server = <<74,125,239,134>>}}, C),
    true = match_one_filter(#session_update{session = #session{ port = 80,
				      client = <<10,10,10,1>>,
				      server = <<74,125,239,134>>}}, C).

c1() ->
    Client1 = #network{ipAddress = <<10,10,10,1>>, mask = 30},
    Server1 = #network{ipAddress = <<74,125,238,134>>, mask = 32},
    POR1 = #portOrRange{range = #range{startItem = 79,
					  endItem = 81}},
    #condition{portOrRange =  POR1,
	       client = Client1,
	       server = Server1 }.

c2() ->
    Client2 = #network{ipAddress = <<10,10,10,1>>, mask = 30},
    Server2 = #network{ipAddress = <<74,125,239,134>>, mask = 32},
    POR2 = #portOrRange{port = #compare{op = ?api_Op_LESS, 
					port = 80}},
    #condition{portOrRange =  POR2,
		  client = Client2,
		  server =  Server2}.
    
filters() ->
     Client3 = #network{ipAddress = <<10,10,10,1>>, mask = 30},
    Server3 = #network{ipAddress = <<74,125,239,134>>, mask = 32},
    POR3 = #portOrRange{range = #range{startItem = 79,
					  endItem = 81}},
    C3 = #condition{portOrRange =  POR3,
		  client = Client3,
		  server = Server3 ,
		   connectionLatency = 1500,
		   latency = 2000},
    [#filter{id = 1, condition = c1()},
     #filter{id = 10, condition = c2()},
     #filter{id = 5, condition = C3},
     #filter{id = 8, condition = c1()}].
    

match_test() ->
    none = match(#session_update{session = #session{ port = 80,
				    client = <<10,10,10,1>>,
				    server = <<74,125,239,134>> },
				 latency = 1200},
		 filters()),
     none =  match(#session_update{session = #session{ port = 80,
				    client = <<10,10,10,1>>,
				    server = <<74,125,239,134>> },
				 latency = 1600},
		 filters()),
     #filter{id = 5} =  
	match(#session_update{session 
			      = #session{ port = 80,
					  client = <<10,10,10,1>>,
					  server = <<74,125,239,134>>},
			      latency = 1600,
			      transactions 
			      = [#transaction{},
				 #transaction{latency = 3000}]},
	      filters()).
