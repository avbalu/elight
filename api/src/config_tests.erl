%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc
%%%
%%% @end
%%% Created : 28 Jan 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(config_tests).

-import(config, [del_filters/2, add_filters/2, dbs/0]).

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
    Port = configdb:get(config_tcp_port),
    {ok, C0} = thrift_client_util:new( "localhost", 
				       Port, 
				       config_thrift,[]),
    C0.
   
cleanup(X) ->
    test:cleanup(X).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

test(C0) ->
    [?_test(ping_t(C0)),
     ?_test(add_filters_t(C0)),
     ?_test(add_filters_t(C0)),
     ?_test(add_filters_duplicate_t(C0)),
     ?_test(add_filters_input_duplicate_t(C0)),
     ?_test(del_filters_t(C0)),
     ?_test(get_filters_empty_t(C0)),
     ?_test(get_filters_t(C0)),
     ?_test(add_filters_events_t(C0))].

 
%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%


ping_t(C0) ->
     {C1, {ok, <<"pong">>} } = thrift_client:call (C0, ping, []),
    {_C2, {ok, <<"pong">>} } = thrift_client:call (C1, ping, []).

add_filters_t(C0) ->   
    Filters = [#filter{id = 1, condition = #condition{client = #network{ipAddress = <<10,0,0,8>>,mask = 32}}, action = #action{logSession = 3}}],
    [ begin 
	  catch del_filters(X, [1]),
	  thrift_client:call(C0, add_filters,[X, Filters])
      end || X <- dbs()].

add_filters_duplicate_t(C0) ->    
    Filters = [#filter{id = 1, condition = #condition{client = #network{ipAddress = <<10,0,0,1>>,mask = 32}}, action = #action{logSession = 3}}],    
    [ begin catch add_filters(X, Filters),
	    {_C1, {exception,#duplicateEntry{object = <<"[1]">>}}}
		= ( catch thrift_client:call(C0, add_filters,[X, Filters]))
      end || X <- dbs()].


add_filters_input_duplicate_t(C0) ->
    Filters = [#filter{id = X} || X <- [5,6,7,5,6]],
    [{_C1, {exception,{invalidInput,<<"Duplicate Filter Ids [5,6] in input">>}}}
     = (catch thrift_client:call(C0, add_filters,[X, Filters]))
     || X <- dbs() ]. 
    
del_filters_t(C0) ->
   
    Filters = [#filter{id = 1, condition = #condition{client = #network{ipAddress = <<10,0,0,1>>,mask = 32}}, action = #action{logSession = 3}}],
    [ begin catch add_filters(X, Filters),
	    thrift_client:call(C0, del_filters,[[1]])
      end || X <- dbs() ].


get_filters_empty_t(C0) ->
    F = fun(Db, Cin) -> 
		{Cout, _R} = thrift_client:call(Cin, get_filters,[Db]),
		Cout
	end,
    lists:foldl(F, C0, dbs()).

get_filters_t(C0) ->
    Filters = [#filter{id = 1, condition = #condition{client = #network{ipAddress = <<10,0,0,8>>,mask = 32}}, action = #action{logSession = 3}}],    
    [ catch add_filters(X, Filters)
     || X <- dbs() ],
    get_filters_empty_t(C0).

add_filters_events_t(C0) ->   
    Filters = [#filter{id = 1, condition 
		       = #condition{client 
				    = #network{ipAddress = <<10,0,0,8>>,
					       mask = 32}}, 
		       action = #action{logSession = 3, notify = 2}}],
    [ begin 
	  catch del_filters(X, [1]),
	  thrift_client:call(C0, add_filters,[X, Filters])
      end || X <- dbs()].


