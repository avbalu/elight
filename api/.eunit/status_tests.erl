%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc
%%%
%%% @end
%%% Created : 28 Jan 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(status_tests).

-export([setup/0]).

-include("../../common/common.hrl").

-include_lib("eunit/include/eunit.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
 
unit_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun test/1
    }.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
 


setup() ->
    code:add_path("../../common/ebin"),
    test:setup(),
    Port = configdb:get(status_tcp_port),
    {ok, C0} = thrift_client_util:new( "localhost", 
				       Port, 
				       status_thrift,[]),
    C0.
   
cleanup(X) ->
    test:cleanup(X).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%


test(C0) ->
    [?_test(ping_t(C0)),
     ?_test(clientsUsing_noData_t(C0)),
     ?_test(serversUsedBy_noData_t(C0)),
     ?_test(hosts_test(C0)),
     ?_test(groups_test(C0)),
     ?_test(hostProfile_noEntry_test(C0)),
     ?_test(hostProfile_invalidInput_test(C0)),
     ?_test(hostProfile_test(C0)),
     ?_test(serviceProfileByClient_test(C0)),
     ?_test(serviceProfileByClient_invalidInput_test(C0)),
     ?_test(serviceProfileByClient_noEntry_test(C0)),
     ?_test(servicePerformanceByClient_noData_test(C0)),
     ?_test(servicePerformanceByClient_test(C0)),
     ?_test(hostProfiles_test(C0)),
     ?_test(servicePerformance_test(C0)),
     ?_test(servicePerformance_noData_test(C0))].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

ping_t(C0) ->
     {C1, {ok, <<"pong">>} } = thrift_client:call (C0, ping, []),
    {_C2, {ok, <<"pong">>} } = thrift_client:call (C1, ping, []).

clientsUsing_noData_t(C0) ->
    {_C1, {ok, []}} = thrift_client:call(C0, clientsUsing, [<<1,1,1,1>>]).

serversUsedBy_noData_t(C0) ->
    {_C1, {ok, []}} = thrift_client:call(C0, serversUsedBy, [<<1,1,1,1>>]).

hosts_test(C0) ->
    {_C1, {ok, _}} = thrift_client:call(C0, hosts, []).

groups_test(C0) ->
    {_C1,{ok, _}} =  thrift_client:call(C0, groups, []).
 

hostProfile_noEntry_test(C0) ->
    
    F = fun(C) -> (catch thrift_client:call (C, 
					    hostProfile, 
					    [<<1,2,3,4>>])) 
	end,
    {_C1, {exception,#noEntry{}}} = F(C0). 
 
hostProfile_invalidInput_test(C0) ->
    
    F = fun(C) -> (catch thrift_client:call (C, 
					    hostProfile, 
					    [<<74,125,239>>])) 
	end,
    {_C1, {exception,#invalidInput{}}} = F(C0). 

hostProfile_test(C0) ->
    
    F = fun(C) -> (catch thrift_client:call (C, 
					    hostProfile, 
					    [<<74,125,239,134>>])) 
	end,
{_C1, {ok, #hostProfile{}}} = F(C0).    

serviceProfileByClient_test(C0) ->
    
    {_C1, {ok, _} } =  thrift_client:call (C0, serviceProfileByClient,
					   [<<10,0,0,8>>,
					    <<74,125,239,134>>,
					   #serviceId{port = 80, 
						      proto = 6}]).
    
serviceProfileByClient_invalidInput_test(C0) ->
    
    try 
	thrift_client:call (C0, serviceProfileByClient,
			    [<<10,0,0,8>>,
			     <<74,125,239,134>>,
			     #serviceId{port = 80, 
					proto = 8}]) of
	_ -> ok
    catch throw:{_NewClient, {exception, #invalidInput{}}} -> ok
    end.

serviceProfileByClient_noEntry_test(C0) ->
    
    try thrift_client:call (C0, serviceProfileByClient,
			    [<<10,0,0,8>>,
			     <<74,125,239,134>>,
			     #serviceId{port = 81, 
					proto = 6}]) of
	_ ->
	    ok
    catch
	throw:{_NewClient, {exception, #noEntry{}}} ->
	    ok
    end.

servicePerformanceByClient_noData_test(C0) ->
   
 	C = <<1,1,1,1>>, S = <<2,2,2,2>>, P = 80,
{_C1, {ok,[]}} = thrift_client:call(C0, servicePerformanceByClient,
	 [C, S, #serviceId{port = P}]).

servicePerformanceByClient_test(C0) ->
  
 {_C1, {ok, _}} = thrift_client:call(C0, servicePerformanceByClient,[<<10,0,0,8>>,<<74,125,239,134>>,#serviceId{port = 80, proto = 6}]).

hostProfiles_test(C0) -> 

{_C1, {ok, _}} = thrift_client:call(C0, hostProfiles, []).

servicePerformance_test(C0) ->

{_C1, {ok, X}} = thrift_client:call(C0, servicePerformance, [<<74,125,239,134>>,#serviceId{port=80}]),
true = lists:all(fun(Y) -> is_record(Y,performanceData) end, X).

servicePerformance_noData_test(C0) ->

{_C1, {ok, []}} = thrift_client:call(C0, servicePerformance, [<<74,125,239,134>>,#serviceId{port=8}]).
