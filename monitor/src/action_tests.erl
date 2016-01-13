%%%-------------------------------------------------------------------
%%% @author adoor balasubramanian <balu@localhost.localdomain>
%%% @copyright (C) 2015, adoor balasubramanian
%%% @doc
%%%
%%% @end
%%% Created : 15 Feb 2015 by adoor balasubramanian <balu@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(action_tests).


-include_lib("eunit/include/eunit.hrl").
-include("../../api/gen-erl/api_types.hrl").
-include("../../filer/include/filer.hrl").
-include("../../common/common.hrl").
-include("../../api/include/event_mgr.hrl").

-compile(export_all).

-import(action, [act/3]).
-import(config, [del_filters/2, add_filters/2, dbs/0]).

setup() ->
    code:add_path("../../common/ebin"),
    test:setup(),
    Port = configdb:get(config_tcp_port),
    {ok, C0} = thrift_client_util:new( "localhost", 
				       Port, 
				       config_thrift,[]),
    Filters = [#filter{id = 1, condition 
		       = #condition{client 
				    = #network{ipAddress = <<10,0,0,8>>,
					       mask = 32}}, 
		       action = #action{logSession = 1, notify = 1}}],
    [ begin 
	  catch del_filters(X, [1]),
	  thrift_client:call(C0, add_filters,[X, Filters])
      end || X <- dbs()],
    C0.

cleanup(X) ->
    test:cleanup(X).

unit_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun test/1
    }.

test(_C0) ->
    ?_test(test()).

test() ->
    act(#session{client = <<10,10,10,1>>, 
			server = <<192,168,0,1>>,
			port = 80},
	       [],
	       #filter{id = 1,
		       action = #action{min = 0,
					logSession = 1,
					notify = 100}}).
